module Components.NavBar

open Bolero
open Bolero.Html
open Web.Routing

type NavBarModel = {
    Items: NavItem list
}

type NavBar() =
    inherit ElmishComponent<NavBarModel, unit>()

    override this.View model _ =
        let createMenuItem item =
            let baseClass = "navbar-item has-text-white has-text-weight-semibold"
            let itemClass = if item.IsActive then baseClass + " is-active" else baseClass
            a {
                attr.``class`` itemClass
                attr.href item.Url
                text item.Label
            }

        nav {
            attr.``class`` "navbar"
            attr.aria "label" "main navigation"
            attr.aria "role" "navigation"
            div {
                attr.``class`` "navbar-start"
                concat {
                    for item in model.Items -> createMenuItem item
                }
            }
        }