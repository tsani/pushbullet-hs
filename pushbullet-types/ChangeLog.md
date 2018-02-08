# Revision history for pushbullet-types

## 0.4.0.1  -- 2018-02-08

* Drop upper bounds on dependencies.

## 0.4.0.0  -- 2017-08-01

* Make `body` field of link pushes optional. Many channel pushes do not include
  a body, but only the URL and the title.
* Improve `Push` parsing. Push sender and receiver information are factored out
  into separate datatypes, and parsed as a whole. Either you have a full sender
  (either a client or a channel) or you don't have any of its fields.  Same
  applies to receivers, except that only clients can be receivers, and don't
  have names. This fixes a bug where pushes sent by channels could not be
  parsed, since such pushes omit certain fields, such as `sender_iden` and
  `sender_email`.

## 0.3.0.0  -- 2017-07-29

* `PushEphemeral` now supports the `targets` field, which allows to more
  faithfully parse clipboard synchronization messages in the realtime event
  stream.
* Missing `Eq` and `Show` instances for `Ephemeral` are now derived.
* Missing device-related datatypes and constructors are now exported.
* Values commonly used by PushBullet for the DeviceIcon type now have a bunch
  of constants exported for them.
* Aeson version upper bound is now higher; this version of pushbullet-types
  will now build with Stackage resolver lts-9.0.
* Misc code cleanup.

## 0.2.0.0  -- 2017-04-30

* Some fields in `PushData` are now made optional, since the objects received
  from Pushbullet may have them missing.
* A minimum value for `PushbulletTime` is added. This is just zero seconds
  since the POSIX epoch.
* Ephemerals are reworked. Now `Ephemeral` actually has one constructor per
  ephemeral type, and the previous constructors of `Ephemeral` are moved into
  `PushEphemeral`. `Ephemeral` now has a constructor for tickles and nops.
  Only push-type tickles have a dedicated constructor in `TickleType`.

## 0.1.0.0  -- 2017-02-20

Define the following core types:

* `Push` -- messages used internally in Pushbullet.
* `Ephemeral` -- messages sent directly into the live event stream. These have
  a multitude of uses, e.g. sending SMS via a phone or seeing whether
  notifications have occurred on a device.
* `Device` -- representation of a device.
* `User` -- representation of a user.
* `Permanant` -- this is an undocumented type in the Pushbullet API. Notably,
  SMS threads and messages are permanents.

Some of these types come in two flavors: `New` and `Existing`. This is
signalled by a type index coming from a datakind.
