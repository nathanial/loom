/-
  Loom.Stencil - Template engine integration for Loom

  Provides file-based template rendering with hot reloading for development.

  ## Setup

  ```lean
  import Loom
  import Loom.Stencil

  def app := Loom.app config
    |>.withStencil { templateDir := "templates", hotReload := true }

  def main : IO Unit := app.run
  ```

  ## Usage in Handlers

  ```lean
  view showUser "/users/:id" do
    let user ← getUser id
    render "users/show" (.object #[("user", userToValue user)])
  ```

  ## Template Directory Structure

  ```
  templates/
  ├── layouts/
  │   └── application.stencil    -- Layout templates
  ├── partials/
  │   └── _header.stencil        -- Partials (start with _)
  └── users/
      ├── index.stencil          -- users/index
      └── show.stencil           -- users/show
  ```

  ## Template Access to Loom Context

  Templates automatically have access to:
  - `{{params.id}}` - URL/form parameters
  - `{{session.user_id}}` - Session data
  - `{{flash.error}}` - Flash messages
  - `{{csrfToken}}` - CSRF token
  - `{{request.path}}` - Request path
-/
import Loom.Stencil.Config
import Loom.Stencil.Manager
import Loom.Stencil.Convert
import Loom.Stencil.ActionM

namespace Loom

-- Re-export main types
export Stencil (Config Manager RenderError)

-- Re-export ActionM helpers
namespace ActionM
  export Stencil.ActionM (render renderWithLayout renderInline renderPartial getStencilManager)
end ActionM

end Loom
