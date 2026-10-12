#!/usr/bin/env bun
import assert from "node:assert/strict";
import { Browser, VirtualConsoleLogLevelEnum } from "happy-dom";

assert.equal(process.argv.length, 3, "Usage: bun scripts/check-webui.mjs http://127.0.0.1:PORT/");
const url = new URL(process.argv[2]);
assert.equal(url.protocol, "http:", "Only a local HTTP server may be checked");
assert.equal(url.hostname, "127.0.0.1", "Only a local HTTP server may be checked");

// Execute only our own built assets: Happy DOM is not a security sandbox.
const browser = new Browser({
  settings: {
    enableJavaScriptEvaluation: true,
    enableCSSFileLoading: true,
  },
});
const deadline = setTimeout(() => {
  console.error("Web UI smoke check timed out after 10 seconds");
  process.exit(1);
}, 10_000);

try {
  const page = browser.newPage();
  const response = await page.goto(url.href);
  assert.equal(response?.status, 200, "The executable must serve its embedded index.html");
  await page.waitUntilComplete();

  const errors = page.virtualConsolePrinter.readAsString(VirtualConsoleLogLevelEnum.error);
  assert.equal(errors, "", `Web UI execution failed:\n${errors}`);

  // A successful HTTP response or an empty #root does not prove React mounted.
  const form = page.mainFrame.document.querySelector("#root form");
  assert.ok(form, "The embedded JavaScript must render the session join form");
  const inputs = [...form.querySelectorAll("input")].filter(input => input.type === "text");
  assert.equal(inputs.length, 2, "The join form must offer session link and display name inputs");
  assert.ok(inputs.every(input => !input.disabled), "The join form inputs must be enabled");
  const submit = form.querySelector('button[type="submit"]');
  assert.ok(submit && !submit.disabled, "The join form must offer an enabled connect button");
  console.log("Web UI smoke check passed: embedded assets execute and the join form renders");
} finally {
  await browser.close();
  clearTimeout(deadline);
}
