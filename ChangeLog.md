# Revision history for pushbullet-types

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
