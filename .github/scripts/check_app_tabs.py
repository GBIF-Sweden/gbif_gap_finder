#!/usr/bin/env python3
"""Opens every tab of a running Gap Finder app in a headless browser and reports the tabs
that do not render.

    python3 .github/scripts/check_app_tabs.py http://127.0.0.1:3838/

The app's page answers as soon as R has read app.R. Its server code, and with it the data
bundle, only runs when a browser connects, and a tab only renders when it is opened. So
this opens each tab of the main tab set, and each tab of a tab set inside it, and checks
that the tab comes to rest, shows no error, and that none of its plots, maps and tables
stays empty. The app leaves an output empty when the bundle lacks the table behind it. A
text or a number that stays empty is counted on the tab's line and does not fail the
check: some are empty until a filter is chosen.

Prints one line per tab. A line that starts with "PROBLEM: " names something that failed;
the exit status is 1 if there is one, otherwise 0. Called by check_app_image.sh.

Needs Playwright (pip install -r .github/scripts/requirements.txt) and a browser:
  CHECK_BROWSER_CHANNEL  the browser Playwright starts. Default "chrome": the Google
                         Chrome on GitHub's runners. Empty: Playwright's own Chromium
                         (python3 -m playwright install chromium).
  CHECK_TAB_TIMEOUT      seconds a tab may take to come to rest. Default 120.
"""
import os
import sys
import time
from urllib.parse import urlsplit

from playwright.sync_api import sync_playwright

URL = sys.argv[1] if len(sys.argv) > 1 else "http://127.0.0.1:3838/"
CHANNEL = os.environ.get("CHECK_BROWSER_CHANNEL", "chrome")
TAB_TIMEOUT = float(os.environ.get("CHECK_TAB_TIMEOUT", "120"))
ORIGIN = "{0.scheme}://{0.netloc}".format(urlsplit(URL))

# The kinds of request that are files of the app itself.
STATIC = ("document", "stylesheet", "script", "font")

problems = []


def problem(text):
    if text not in problems:
        problems.append(text)
        print("PROBLEM: " + text, flush=True)


# Shiny marks the page while the server is working and an output while it is being
# redrawn. Outputs on tabs that are not open keep that mark, so only visible ones count.
BUSY = """() => document.documentElement.classList.contains('shiny-busy') ||
  [...document.querySelectorAll('.recalculating')].some(e => e.offsetWidth || e.offsetHeight)"""

# What the open tab shows: its outputs, the ones that failed and the ones that are empty.
# A validation message ("select a species first") is the app's own text, not a failure.
# A plot or map that failed has its message in an element of its own, right after the
# output; such an output counts as failed, not as empty.
STATE = """() => {
  const vis = e => !!(e.offsetWidth || e.offsetHeight || e.getClientRects().length);
  const pane = [...document.querySelectorAll('.tab-content > .tab-pane.active')].find(vis) || document.body;
  const outputs = [...pane.querySelectorAll('.shiny-bound-output')].filter(vis);
  const failed = e => !!e.nextElementSibling && e.nextElementSibling.classList.contains('shiny-output-error');
  const empty = outputs.filter(e => e.childElementCount === 0 && e.textContent.trim() === '' && !failed(e));
  const drawn = e => e.matches('.html-widget-output, .shiny-plot-output, .shiny-image-output, .shiny-datatable-output');
  return {
    outputs: outputs.length,
    empty_drawn: empty.filter(drawn).map(e => e.id),
    empty_other: empty.filter(e => !drawn(e)).map(e => e.id),
    errors: [...pane.querySelectorAll('.shiny-output-error')]
      .filter(e => vis(e) && !e.classList.contains('shiny-output-error-validation'))
      .map(e => (e.id || (e.previousElementSibling || {}).id || 'an output') + ': ' +
                e.innerText.trim().replace(/\\s+/g, ' ').slice(0, 200)),
    disconnected: !!document.getElementById('shiny-disconnected-overlay')
  };
}"""

# The tabs of the tab sets inside the open tab, as [selector of the tab set, value of
# the tab]. The tab that is open already has been checked with the tab around it.
SUBTABS = """() => {
  const vis = e => !!(e.offsetWidth || e.offsetHeight || e.getClientRects().length);
  const pane = [...document.querySelectorAll('.tab-content > .tab-pane.active')].find(vis);
  if (!pane) return [];
  const open = a => a.classList.contains('active') || a.parentElement.classList.contains('active');
  return [...pane.querySelectorAll('ul.nav[data-tabsetid] a[data-value]')].filter(a => vis(a) && !open(a))
    .map(a => ['ul.nav[data-tabsetid="' + a.closest('ul').dataset.tabsetid + '"]', a.dataset.value]);
}"""


def at_rest(page):
    """True once nothing visible has been busy for 1.5 s; False after TAB_TIMEOUT."""
    end, quiet = time.time() + TAB_TIMEOUT, None
    while time.time() < end:
        if page.evaluate(BUSY):
            quiet = None
        elif quiet is None:
            quiet = time.time()
        elif time.time() - quiet >= 1.5:
            return True
        page.wait_for_timeout(250)
    return False


def check_tab(page, label):
    """Checks the tab that is open now. Returns False when the app can do no more: the
    session has ended, or R is still busy and would be for the next tab as well."""
    t0 = time.time()
    rested = at_rest(page)
    state = page.evaluate(STATE)
    n_empty = len(state["empty_drawn"]) + len(state["empty_other"])
    names = ": " + ", ".join(state["empty_drawn"] + state["empty_other"]) if n_empty else ""
    print(f"tab {label}: {state['outputs']} outputs, {n_empty} empty{names}, {time.time() - t0:.1f} s", flush=True)
    if state["disconnected"]:
        problem(f"tab {label}: the app closed the session (R stopped, or the server code failed)")
        return False
    for err in state["errors"]:
        problem(f"tab {label}: {err}")
    if not rested:
        problem(f"tab {label}: still busy after {TAB_TIMEOUT:.0f} s; the tabs after it were not opened")
        return False
    if state["outputs"] == 0:
        problem(f"tab {label}: shows no output")
    for name in state["empty_drawn"]:
        problem(f"tab {label}: {name} stays empty (a plot, map or table)")
    return True


def main():
    with sync_playwright() as pw:
        browser = pw.chromium.launch(channel=CHANNEL) if CHANNEL else pw.chromium.launch()
        page = browser.new_page(viewport={"width": 1400, "height": 1000})
        page.on("pageerror", lambda e: problem("JavaScript error on the page: " + str(e).splitlines()[0][:200]))
        # Something of the app itself that does not load: an error status from the
        # app, or a stylesheet, script or font that is not there. Map tiles and other
        # outside addresses are not the image's to answer for, and a data request that
        # the page itself cancels (a table that is redrawn) is not a failure.
        page.on("response", lambda r: problem(f"HTTP {r.status} for {r.url[:160]}")
                if r.status >= 400 and r.url.startswith(ORIGIN) else None)
        page.on("requestfailed", lambda r: problem(f"{r.url[:160]} did not load ({r.failure})")
                if r.url.startswith(ORIGIN) and r.resource_type in STATIC else None)
        page.goto(URL, wait_until="domcontentloaded", timeout=120_000)
        try:
            page.wait_for_selector("#main_tabs a[data-value]", timeout=120_000)
        except Exception:
            problem("the page has no tab set (#main_tabs)")
            browser.close()
            return
        tabs = page.evaluate("""() => [...document.querySelectorAll('#main_tabs a[data-value]')]
          .filter(a => a.offsetWidth || a.offsetHeight).map(a => a.dataset.value)""")
        print(f"{page.title()}: {len(tabs)} tabs", flush=True)
        opened, alive = 0, True
        for tab in tabs:
            if not alive:
                break
            try:
                page.click(f'#main_tabs a[data-value="{tab}"]', timeout=20_000)
            except Exception as e:
                problem(f"tab {tab}: could not be opened ({str(e).splitlines()[0][:120]})")
                continue
            alive = check_tab(page, tab)
            opened += 1
            for tabset, sub in (page.evaluate(SUBTABS) if alive else []):
                try:
                    page.click(f'{tabset} a[data-value="{sub}"]', timeout=20_000)
                except Exception as e:
                    problem(f"tab {tab} > {sub}: could not be opened ({str(e).splitlines()[0][:120]})")
                    continue
                alive = check_tab(page, f"{tab} > {sub}")
                opened += 1
                if not alive:
                    break
        if not tabs:
            problem("the tab set has no tabs")
        print(f"{opened} tabs opened, {len(problems)} problems", flush=True)
        browser.close()


if __name__ == "__main__":
    try:
        main()
    except Exception as e:  # the browser did not start, the page did not load
        problem(f"{type(e).__name__}: {str(e).splitlines()[0][:300]}")
    sys.exit(1 if problems else 0)
