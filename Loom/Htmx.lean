/-
  Loom.Htmx - HTMX integration helpers

  HTMX allows any element to issue HTTP requests and swap content dynamically
  using HTML attributes. This module provides:
  - Request detection (isHtmx, htmxTarget, etc.)
  - Response helpers with HTMX-specific headers (htmxRedirect, htmlWithTrigger, etc.)
-/
import Loom.Controller
import Loom.ActionM
import Citadel

namespace Loom

namespace Context

/-- Check if request is from HTMX (HX-Request header) -/
def isHtmx (ctx : Context) : Bool :=
  ctx.header "HX-Request" == some "true"

/-- Get the current URL the browser was on when HTMX made the request -/
def htmxCurrentUrl (ctx : Context) : Option String :=
  ctx.header "HX-Current-URL"

/-- Get the ID of the target element -/
def htmxTarget (ctx : Context) : Option String :=
  ctx.header "HX-Target"

/-- Get the ID of the triggered element -/
def htmxTrigger (ctx : Context) : Option String :=
  ctx.header "HX-Trigger"

/-- Get the name of the triggered element -/
def htmxTriggerName (ctx : Context) : Option String :=
  ctx.header "HX-Trigger-Name"

/-- Get the user's response to hx-prompt -/
def htmxPrompt (ctx : Context) : Option String :=
  ctx.header "HX-Prompt"

/-- Check if request was via hx-boost -/
def htmxBoosted (ctx : Context) : Bool :=
  ctx.header "HX-Boosted" == some "true"

/-- Check if this is a history restore request -/
def htmxHistoryRestoreRequest (ctx : Context) : Bool :=
  ctx.header "HX-History-Restore-Request" == some "true"

end Context

namespace ActionM

/-- Check if current request is from HTMX -/
def isHtmx : ActionM Bool := do
  let ctx ← get
  pure ctx.isHtmx

/-- Get HTMX target element ID from request -/
def htmxTarget : ActionM (Option String) := do
  let ctx ← get
  pure ctx.htmxTarget

/-- Get HTMX trigger element ID from request -/
def htmxTrigger : ActionM (Option String) := do
  let ctx ← get
  pure ctx.htmxTrigger

/-- Get HTMX prompt response from request -/
def htmxPrompt : ActionM (Option String) := do
  let ctx ← get
  pure ctx.htmxPrompt

/-- Return HTML fragment (same as html, semantic alias for partial responses) -/
def htmlFragment (content : String) : ActionM Herald.Core.Response :=
  pure (Citadel.Response.html content)

/-- Create a response with HX-Redirect header (client-side redirect) -/
def htmxRedirect (url : String) : ActionM Herald.Core.Response :=
  pure (Citadel.ResponseBuilder.withStatus Herald.Core.StatusCode.ok
    |>.withHeader "HX-Redirect" url
    |>.build)

/-- Create a response that triggers full page refresh -/
def htmxRefresh : ActionM Herald.Core.Response :=
  pure (Citadel.ResponseBuilder.withStatus Herald.Core.StatusCode.ok
    |>.withHeader "HX-Refresh" "true"
    |>.build)

/-- Create HTML response with HX-Retarget header -/
def htmlWithRetarget (content : String) (selector : String) : ActionM Herald.Core.Response :=
  pure (Citadel.ResponseBuilder.withStatus Herald.Core.StatusCode.ok
    |>.withText content
    |>.withContentType "text/html; charset=utf-8"
    |>.withHeader "HX-Retarget" selector
    |>.build)

/-- Create HTML response with HX-Reswap header -/
def htmlWithReswap (content : String) (strategy : String) : ActionM Herald.Core.Response :=
  pure (Citadel.ResponseBuilder.withStatus Herald.Core.StatusCode.ok
    |>.withText content
    |>.withContentType "text/html; charset=utf-8"
    |>.withHeader "HX-Reswap" strategy
    |>.build)

/-- Create HTML response with HX-Trigger header (triggers client-side event) -/
def htmlWithTrigger (content : String) (event : String) : ActionM Herald.Core.Response :=
  pure (Citadel.ResponseBuilder.withStatus Herald.Core.StatusCode.ok
    |>.withText content
    |>.withContentType "text/html; charset=utf-8"
    |>.withHeader "HX-Trigger" event
    |>.build)

/-- Create HTML response with HX-Trigger-After-Settle header -/
def htmlWithTriggerAfterSettle (content : String) (event : String) : ActionM Herald.Core.Response :=
  pure (Citadel.ResponseBuilder.withStatus Herald.Core.StatusCode.ok
    |>.withText content
    |>.withContentType "text/html; charset=utf-8"
    |>.withHeader "HX-Trigger-After-Settle" event
    |>.build)

/-- Create HTML response with HX-Trigger-After-Swap header -/
def htmlWithTriggerAfterSwap (content : String) (event : String) : ActionM Herald.Core.Response :=
  pure (Citadel.ResponseBuilder.withStatus Herald.Core.StatusCode.ok
    |>.withText content
    |>.withContentType "text/html; charset=utf-8"
    |>.withHeader "HX-Trigger-After-Swap" event
    |>.build)

/-- Create HTML response with HX-Push-Url header -/
def htmlWithPushUrl (content : String) (url : String) : ActionM Herald.Core.Response :=
  pure (Citadel.ResponseBuilder.withStatus Herald.Core.StatusCode.ok
    |>.withText content
    |>.withContentType "text/html; charset=utf-8"
    |>.withHeader "HX-Push-Url" url
    |>.build)

/-- Create HTML response with HX-Replace-Url header -/
def htmlWithReplaceUrl (content : String) (url : String) : ActionM Herald.Core.Response :=
  pure (Citadel.ResponseBuilder.withStatus Herald.Core.StatusCode.ok
    |>.withText content
    |>.withContentType "text/html; charset=utf-8"
    |>.withHeader "HX-Replace-Url" url
    |>.build)

/-- Create a 286 Stop Polling response -/
def htmxStopPolling : ActionM Herald.Core.Response :=
  pure (Citadel.ResponseBuilder.withStatus (Herald.Core.StatusCode.mk 286)
    |>.build)

end ActionM

end Loom
