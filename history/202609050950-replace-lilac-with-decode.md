# Replace Lilac validation with typed decode

- Remove the Lilac dependency, module entry, imports, and runtime `dev-check` call.
- Define closed `StirPageResources` and `StirScriptResource` structs for page configuration.
- Decode public map inputs with `decode-map-as`, including a second typed boundary for script configuration objects.
- Give `make-page` an explicit Dynamic-boundary-to-String function contract.
- Keep explicit `Dynamic` leaves only for rendered content and the legacy string-or-object scripts collection.
- Add the previously supported `:viewport` option to the closed page resource shape.
