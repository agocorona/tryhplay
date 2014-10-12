import Haste.HPlay.View

main= runBody $ test

test=  getInt Nothing `wcallback` const test