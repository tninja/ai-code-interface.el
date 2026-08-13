If any program code changes, run unit-tests and follow up on the test-result (fix code if there is an error).
Prefer a small set of high-value tests. Cover only distinct behaviors, important edge cases, or regressions that materially increase confidence.
Do not add low-value or duplicate tests.
If the tests use random values (for example random numbers or UUIDs), make them reproducible by fixing the random seed or replacing them with deterministic fixtures.
Before editing, record a diagnostics baseline by calling the diagnostics_baseline MCP tool. After each edit, call the get_diagnostics MCP tool with since="baseline" for the touched files and do not finish until its status is "clean" (no new diagnostics versus the baseline).
