# Loom Roadmap

This document outlines feature proposals, code improvements, and cleanup tasks for the Loom web framework.

---

## Feature Proposals

### [Priority: High] WebSocket Support

**Description:** Add WebSocket protocol support alongside the existing SSE (Server-Sent Events) implementation for full-duplex real-time communication.

**Rationale:** SSE is unidirectional (server to client). WebSockets would enable bidirectional real-time features like chat, collaborative editing, and real-time form validation. This is a natural extension of the existing SSE infrastructure.

**Affected Files:**
- New file: `Loom/WebSocket.lean`
- `Loom/App.lean` - Add WebSocket route registration
- `Loom/ActionM.lean` - Add WebSocket message sending helpers

**Estimated Effort:** Large

**Dependencies:** Requires citadel to implement WebSocket upgrade handling first

---

### [Priority: High] Request Validation Framework

**Description:** Add a declarative validation framework for request parameters with type-safe error messages.

**Rationale:** Currently, parameter validation is done manually in each action. A validation framework would reduce boilerplate, ensure consistent error handling, and enable automatic form error rendering.

**Affected Files:**
- New file: `Loom/Validation.lean`
- `Loom/ActionM.lean` - Add validation integration
- `Loom/Form.lean` - Extend with validation helpers

**Estimated Effort:** Medium

**Dependencies:** None

**Example API:**
```lean
def createUser : ActionM Response := do
  let validated ← validate do
    required "email" |> email
    required "password" |> minLength 8
    optional "name"
  match validated with
  | .ok params => -- proceed with validated params
  | .errors errs => badRequest (renderErrors errs)
```

---

### [Priority: High] Response Streaming Support

**Description:** Add support for streaming responses (chunked transfer encoding) for large file downloads, report generation, and AI streaming responses.

**Rationale:** Currently all responses are buffered in memory. Streaming would enable efficient handling of large responses without memory pressure.

**Affected Files:**
- `Loom/ActionM.lean` - Add streaming response builders
- `Loom/Controller.lean` - Add streaming types
- New file: `Loom/Stream.lean`

**Estimated Effort:** Medium

**Dependencies:** May require citadel changes for chunked encoding

---

### [Priority: Medium] Database Connection Pooling

**Description:** Add connection pooling for persistent database connections to handle concurrent requests more efficiently.

**Rationale:** The current implementation uses a single shared `IO.Ref` for the persistent connection. Under high concurrency, this could become a bottleneck. A proper connection pool would improve throughput.

**Affected Files:**
- `Loom/Database.lean` - Add pool configuration and management
- `Loom/App.lean` - Integrate pool lifecycle

**Estimated Effort:** Medium

**Dependencies:** None

---

### [Priority: Medium] Internationalization (i18n) Support

**Description:** Add built-in support for internationalization with locale detection, message catalogs, and pluralization.

**Rationale:** Web applications often need to support multiple languages. A built-in i18n system would make Loom more suitable for international projects.

**Affected Files:**
- New file: `Loom/I18n.lean`
- `Loom/Context.lean` - Add locale to context
- `Loom/ActionM.lean` - Add translation helpers

**Estimated Effort:** Medium

**Dependencies:** None

---

### [Priority: Medium] Asset Pipeline

**Description:** Add an asset pipeline for compiling, minifying, and fingerprinting CSS/JS assets with cache-busting URLs.

**Rationale:** Modern web apps need asset compilation (SCSS, TypeScript) and cache-busting. Currently, static files are served as-is without processing.

**Affected Files:**
- New file: `Loom/Assets.lean`
- `Loom/Static.lean` - Extend with asset manifest support
- `Loom/App.lean` - Add asset configuration

**Estimated Effort:** Large

**Dependencies:** External tooling (esbuild, sass) via FFI or subprocess

---

### [Priority: Medium] API Versioning

**Description:** Add built-in support for API versioning through URL prefixes, headers, or query parameters.

**Rationale:** APIs evolve over time. Built-in versioning support would make it easier to maintain backward compatibility while adding new features.

**Affected Files:**
- `Loom/Router.lean` - Add versioned route groups
- `Loom/App.lean` - Add version configuration
- New file: `Loom/Version.lean`

**Estimated Effort:** Small

**Dependencies:** None

---

### [Priority: Medium] Background Job Processing

**Description:** Add a simple background job queue for async task processing (email sending, report generation, etc.).

**Rationale:** Many web applications need to perform work outside the request-response cycle. A built-in job queue would eliminate the need for external job processors.

**Affected Files:**
- New file: `Loom/Jobs.lean`
- New file: `Loom/Worker.lean`
- `Loom/App.lean` - Add worker configuration

**Estimated Effort:** Large

**Dependencies:** Potentially conduit (channels library) for job queue

---

### [Priority: Low] GraphQL Support

**Description:** Add GraphQL endpoint support with schema definition and query execution.

**Rationale:** GraphQL is popular for APIs. Native support would make Loom viable for GraphQL backends.

**Affected Files:**
- New file: `Loom/GraphQL.lean`
- New file: `Loom/GraphQL/Schema.lean`
- New file: `Loom/GraphQL/Resolver.lean`

**Estimated Effort:** Large

**Dependencies:** GraphQL parser implementation

---

### [Priority: Low] OpenAPI/Swagger Documentation Generation

**Description:** Auto-generate OpenAPI specifications from route definitions.

**Rationale:** API documentation is essential. Automatic generation from route definitions would ensure docs stay in sync with implementation.

**Affected Files:**
- New file: `Loom/OpenAPI.lean`
- `Loom/Router.lean` - Add metadata for documentation

**Estimated Effort:** Medium

**Dependencies:** None

---

## Code Improvements

### [Priority: High] Replace FNV-1a Session Signing with HMAC-SHA256

**Current State:** Session signing uses a custom FNV-1a based hash in `Loom/Session.lean` (lines 9-31). While functional for tamper detection, FNV-1a is not a cryptographic hash.

**Proposed Change:** Replace with HMAC-SHA256 for cryptographically secure session signing. This would require adding FFI bindings to OpenSSL's HMAC functions.

**Benefits:**
- Cryptographically secure session signing
- Industry-standard approach
- Resistant to length extension attacks

**Affected Files:**
- `Loom/Session.lean` - Replace `fnvHash` and `sign` functions
- New file: `ffi/crypto.c` or use existing citadel OpenSSL bindings

**Estimated Effort:** Medium

---

### [Priority: High] Add Request Body Size Limits

**Current State:** No validation of request body size, which could lead to denial-of-service attacks via large request bodies.

**Proposed Change:** Add configurable maximum body size to `AppConfig` and validate before parsing.

**Benefits:**
- Protection against DoS attacks
- Configurable per-application limits
- Better memory management

**Affected Files:**
- `Loom/Controller.lean` - Add `maxBodySize` to `AppConfig`
- `Loom/App.lean` - Add size check in `buildContext`

**Estimated Effort:** Small

---

### [Priority: High] Rate Limiting Implementation

**Current State:** `Middleware.rateLimit` in `Loom/Middleware.lean` (lines 123-127) is a stub that just passes through. The comment acknowledges it needs proper implementation.

**Proposed Change:** Implement actual rate limiting with sliding window or token bucket algorithm. Store counters in memory or optionally in Ledger database.

**Benefits:**
- Protection against abuse
- API quota enforcement
- Configurable per-route or per-user limits

**Affected Files:**
- `Loom/Middleware.lean` - Implement `rateLimit` function

**Estimated Effort:** Medium

---

### [Priority: Medium] Improve Basic Auth Base64 Decoding

**Current State:** The `basicAuth` middleware in `Loom/Middleware.lean` (lines 98-121) has a comment noting it does not properly decode base64. It just splits on `:` which won't work.

**Proposed Change:** Add proper base64 decoding, either via FFI or pure Lean implementation.

**Benefits:**
- Working basic authentication
- Standards compliance

**Affected Files:**
- `Loom/Middleware.lean` - Fix `basicAuth` function
- Potentially new file: `Loom/Base64.lean` or use staple

**Estimated Effort:** Small

---

### [Priority: Medium] Add ETag Support for Dynamic Content

**Current State:** ETags are only generated for static files based on modification time (`Loom/Static.lean` line 73). Dynamic responses don't support conditional requests.

**Proposed Change:** Add helpers for generating ETags on dynamic content and handling `If-None-Match` headers.

**Benefits:**
- Reduced bandwidth for unchanged content
- Better caching for API responses
- Faster page loads for unchanged content

**Affected Files:**
- `Loom/ActionM.lean` - Add conditional response helpers
- `Loom/Controller.lean` - Add ETag context helpers

**Estimated Effort:** Small

---

### [Priority: Medium] Timeout Middleware Implementation

**Current State:** `Middleware.timeout` in `Loom/Middleware.lean` (lines 129-133) is a stub. The comment notes it needs async/Task support.

**Proposed Change:** Implement request timeout using Lean's `IO.timeout` or task-based approach.

**Benefits:**
- Protection against slow handlers
- Better resource management
- Graceful degradation under load

**Affected Files:**
- `Loom/Middleware.lean` - Implement `timeout` function

**Estimated Effort:** Medium

---

### [Priority: Medium] Add Content Negotiation

**Current State:** Content type is manually handled. The `acceptsJson` helper in `Loom/Controller.lean` (lines 88-91) is basic.

**Proposed Change:** Add proper content negotiation with q-values parsing and automatic response format selection.

**Benefits:**
- Automatic format selection based on Accept header
- Single handler serving multiple formats
- Better HTTP compliance

**Affected Files:**
- New file: `Loom/ContentNegotiation.lean`
- `Loom/ActionM.lean` - Add negotiation helpers

**Estimated Effort:** Medium

---

### [Priority: Medium] Add Compression Middleware

**Current State:** No response compression support.

**Proposed Change:** Add gzip/brotli compression middleware for text responses.

**Benefits:**
- Reduced bandwidth usage
- Faster page loads
- Industry standard practice

**Affected Files:**
- `Loom/Middleware.lean` - Add `compression` middleware
- FFI bindings for zlib/brotli

**Estimated Effort:** Medium

---

### [Priority: Low] Add Route Constraints

**Current State:** Route parameters are always strings or require manual parsing in handlers.

**Proposed Change:** Add route constraints for type-safe parameter matching (e.g., `/users/:id(int)` only matches numeric IDs).

**Benefits:**
- Type-safe routing
- Automatic 404 for invalid parameter types
- Cleaner handler code

**Affected Files:**
- `Loom/App.lean` - Extend `matchRoute` function (line 269)
- `Loom/Router.lean` - Add constraint parsing

**Estimated Effort:** Medium

---

### [Priority: Low] Improve Error Messages for Route Mismatches

**Current State:** Failed route matches return generic 404. No indication of what route almost matched or why it failed.

**Proposed Change:** In development mode, show helpful error messages indicating closest matching routes.

**Benefits:**
- Better developer experience
- Faster debugging of routing issues

**Affected Files:**
- `Loom/App.lean` - Extend `findRoute` and `toHandler`

**Estimated Effort:** Small

---

## Code Cleanup

### [Priority: High] Remove Duplicate toSnakeCase Implementations

**Issue:** `toSnakeCase` is implemented in two places:
1. `Loom/Page.lean` (lines 214-221)
2. `Loom/Routing/Macros.lean` (lines 56-66)

**Location:** Both files contain nearly identical implementations.

**Action Required:** Extract to a shared utility module (perhaps `Loom/Utils.lean`) and import from both locations.

**Estimated Effort:** Small

---

### [Priority: Medium] Add Documentation Comments to Public APIs

**Issue:** Many public functions lack doc comments. For example:
- Most `ActionM` methods in `Loom/ActionM.lean`
- `RouteMiddleware` functions in `Loom/Middleware.lean`
- Database helpers in `Loom/Controller.lean`

**Location:** Multiple files throughout the codebase.

**Action Required:** Add `/-- ... -/` doc comments to all public definitions following Lean 4 documentation conventions.

**Estimated Effort:** Medium

---

### [Priority: Medium] Consolidate Response Builder Patterns

**Issue:** Response building is inconsistent. Some places use `Citadel.Response.html`, others use `Citadel.ResponseBuilder`. For example:
- `ActionM.html` uses `Citadel.Response.html` (line 246)
- `ActionM.htmlWithTrigger` uses `Citadel.ResponseBuilder` (lines 106-111)

**Location:** `Loom/ActionM.lean`, `Loom/Htmx.lean`

**Action Required:** Standardize on one approach, preferably `ResponseBuilder` for consistency and extensibility.

**Estimated Effort:** Small

---

### [Priority: Medium] Add Missing Test Coverage

**Issue:** Several modules lack test coverage:
- `Loom/Multipart.lean` - No multipart parsing tests
- `Loom/Htmx.lean` - No HTMX helper tests
- `Loom/Auth.lean` - No auth middleware tests
- `Loom/Audit.lean` - No audit logging tests
- `Loom/SSE.lean` - No SSE publishing tests
- `Loom/Chronicle.lean` - No logging middleware tests

**Location:** `Tests/Main.lean`

**Action Required:** Add test suites for each missing module.

**Estimated Effort:** Medium

---

### [Priority: Medium] Address Skipped Test

**Issue:** Test "decode rejects wrong secret (skipped)" in `Tests/Main.lean` (lines 112-114) is skipped with a TODO comment about implementing proper HMAC-SHA256 signing.

**Location:** `Tests/Main.lean` line 112

**Action Required:** Implement proper HMAC signing (see code improvement above) and enable this test.

**Estimated Effort:** Small (once HMAC is implemented)

---

### [Priority: Low] Remove Backward Compatibility Re-exports

**Issue:** Several modules exist only for backward compatibility:
- `Loom/Json.lean` - Re-exports from `Staple.Json`
- `Loom/IncludeStr.lean` - Re-exports from `Staple`
- `Loom/Transaction.lean` - Re-exports from `Ledger`

**Location:** These three files

**Action Required:** Document deprecation timeline. Eventually remove these modules and update dependent code to import directly from source libraries.

**Estimated Effort:** Small

---

### [Priority: Low] Clean Up Unused toCitadelRouter Function

**Issue:** `Routes.toCitadelRouter` in `Loom/Router.lean` (lines 84-91) creates a router that just returns 404. The comment says "The action will be wrapped by the App to provide context" but it's unclear if this is ever used.

**Location:** `Loom/Router.lean` lines 84-91

**Action Required:** Verify if this function is used anywhere. If not, remove it. If it is, add documentation explaining its purpose.

**Estimated Effort:** Small

---

### [Priority: Low] Improve Error Handling in Multipart Parser

**Issue:** The multipart parser in `Loom/Multipart.lean` silently ignores parsing errors in several places (e.g., line 213 just does `pure ()` on parse failure).

**Location:** `Loom/Multipart.lean` lines 145-228

**Action Required:** Add proper error reporting or logging for malformed multipart data. Consider returning `Except` instead of `Option` for better error information.

**Estimated Effort:** Small

---

### [Priority: Low] Add Type Annotations to Complex Functions

**Issue:** Some complex functions lack explicit type annotations, relying on inference. This can make code harder to understand.

**Location:** Various elaborators in `Loom/Page.lean` and `Loom/Routing/Macros.lean`

**Action Required:** Add explicit type annotations to macro elaborator helper functions.

**Estimated Effort:** Small

---

## Security Improvements

### [Priority: High] Add HSTS Header Support

**Issue:** No built-in support for HTTP Strict Transport Security.

**Location:** `Loom/Middleware.lean`

**Action Required:** Add `hsts` middleware that sets `Strict-Transport-Security` header.

**Estimated Effort:** Small

---

### [Priority: High] Add Content Security Policy Support

**Issue:** No built-in CSP header support.

**Location:** `Loom/Middleware.lean`

**Action Required:** Add configurable CSP middleware.

**Estimated Effort:** Small

---

### [Priority: Medium] Session Expiration

**Issue:** Sessions never expire. The session cookie has no `Max-Age` or `Expires` attribute set.

**Location:** `Loom/Session.lean` lines 122-128, `Loom/App.lean` lines 239-249

**Action Required:** Add configurable session expiration to `AppConfig` and set cookie expiration accordingly.

**Estimated Effort:** Small

---

### [Priority: Medium] Secure Cookie Flag in Production

**Issue:** The `Secure` flag on session cookies is always `false` (`Loom/Session.lean` line 125, inherited by cookie builder).

**Location:** `Loom/Session.lean`, `Loom/App.lean`

**Action Required:** Add production mode detection and automatically set `Secure` flag when using HTTPS.

**Estimated Effort:** Small

---

## Performance Improvements

### [Priority: Medium] Optimize Route Matching

**Issue:** Route matching iterates through all routes and sorts matches every request (`Loom/App.lean` lines 301-314).

**Location:** `Loom/App.lean` `findRoute` function

**Action Required:** Build a trie-based router for O(path length) matching instead of O(number of routes).

**Estimated Effort:** Medium

---

### [Priority: Low] Cache Compiled CSRF Tokens

**Issue:** CSRF token is regenerated for every request even though it depends only on session data.

**Location:** `Loom/Form.lean` lines 66-82

**Action Required:** Cache the token in the session after first generation.

**Estimated Effort:** Small

---

### [Priority: Low] Lazy Session Encoding

**Issue:** Session is always encoded on every response, even if unchanged.

**Location:** `Loom/App.lean` `finalizeResponse` function

**Action Required:** Only encode and set cookie if session was modified (session already tracks `modified` flag).

**Estimated Effort:** Small

---
