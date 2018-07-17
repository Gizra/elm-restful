[![Build Status](https://travis-ci.org/Gizra/elm-restful.svg?branch=master)](https://travis-ci.org/Gizra/elm-restful)

# elm-restful

This package contains modules useful for working a RESTful backend (particularly
as implemented in Drupal) for an Elm application.

- `Restful.Endpoint` provides a way of modelling a Restful endpoint and making
  the usual CRUD requests to it.

- `Restful.Cache` builds on `Restful.Endpoint` by providing a way of tracking
  a cache of items returned from an endpoint, allowing for lazy-loading of
  items as needed.

- `Restful.Login` provides a way of handling login/logout and the caching of
  credentials locally.
