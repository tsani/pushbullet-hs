# Revision history for servant-pushbullet-client

## 0.0.2.0  -- 2017-02-10

Still prerelease. This just adds a bunch of lenses using `microlens-th`.

## 0.0.1.0  -- 2017-02-07

Prerelease version that contains a (small) subset of the Pushbullet API. We
aren't sure yet if all our choices are good, e.g. the use of the `EqT` type
family in conjunction with the `Status` datakind for turning fields on and off
inside datatypes.
