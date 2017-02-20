# Revision history for pushbullet-types

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
