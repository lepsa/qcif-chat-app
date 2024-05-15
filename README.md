# QCIF Chat App

## Overview

- Server side rendering: `blaze-html` and `htmx` for rendering and updating on the client.
- Authentication: Cookies and JWTs for both browsers and other clients.
- API: REST HTML for browsers and JSON+OpenAPI for other clients.
  - HtmlClient :<|> JsonClient
- DB: Sqlite, with few enough constraints that moving over to PostgresQL is possible.