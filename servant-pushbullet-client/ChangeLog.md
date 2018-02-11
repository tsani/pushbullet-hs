# Revision history for servant-pushbullet-client

## 0.5.0.0  -- 2018-02-11

* Require version 0.4.0.2 of pushbullet-types.
* Define a client function for deleting a push.
* Define a client function for getting a single push.
  Note: this API endpoint is undocumented.

## 0.4.0.0  -- 2017-08-01

Bump dependency on pushbullet-types to require version 0.4. This version
includes important bug fixes for pushes sent by channels.

## 0.3.0.0  -- 2017-07-29

Bump dependency on pushbullet-types to require version 0.3.

## 0.2.0.0  -- 2017-04-30

Adjust argument order for a function, so `Cursor` arguments are always last.

## 0.1.0.0  -- 2017-02-20

The bulk of the library has been moved into a separate library
pushbullet-types.

This library now just defines the Pushbullet API-as-a-type and applies
servant-client's `client` function to produce the functions for querying the
Pushbullet API. Also included in this library proper is a utility function for
consuming paginated data returned from Pushbullet.

## 0.0.3.0  -- 2017-02-10

Forgot to export all those lenses.

## 0.0.2.0  -- 2017-02-10

Still prerelease. This just adds a bunch of lenses using `microlens-th`.

## 0.0.1.0  -- 2017-02-07

Prerelease version that contains a (small) subset of the Pushbullet API. We
aren't sure yet if all our choices are good, e.g. the use of the `EqT` type
family in conjunction with the `Status` datakind for turning fields on and off
inside datatypes.
