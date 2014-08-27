-- this example show a image gallery. It advances each 20 seconds and by pressing the button

{-# LANGUAGE DeriveDataTypeable #-}

import Haste.HPlay.View
import Data.Typeable
import Control.Applicative

newtype GalleryIndex= G Int deriving Typeable

main= runBody gallery

gallery :: Widget ()
gallery = p "this example show a image gallery. It advances each 20 seconds and by\
               \ pressing the button" ++>
 (wtimeout 20000 $ do
  G i <- getSData <|> return (G 0)
  let i' = if i == length gall-1  then 0 else  i+1
  setSData $ G i'
  wraw $ do
      img ! src (gall !! i) ! width "100%" ! height "100%"    -- raw Perch code
      br
  submitButton ">" `fire` OnClick
 `wcallback` \_ -> gallery)

  where
  gall=["http://almaer.com/blog/uploads/interview-haskell.png"
       ,"https://encrypted-tbn3.gstatic.com/images?q=tbn:ANd9GcQmmC4kV3NPFIpGL_x4H_iHG_p-c93DGjWfkxVtjxEFVng7A8o-nw"
       ,"https://encrypted-tbn3.gstatic.com/images?q=tbn:ANd9GcQ_ywj-zxDq3h_B4l48XHsjTywrdbK5egxvhxkYJ1HOkDFXd_-H"
       ,"https://encrypted-tbn3.gstatic.com/images?q=tbn:ANd9GcS53axpzkDyzEUAdaIP3YsaHuR-_YqN9qFK3W4bp_D2OBZfW5BU_Q"
       ,"https://encrypted-tbn1.gstatic.com/images?q=tbn:ANd9GcSfP70npv4FOrkBjScP0tVu2t3veSNoFQ6MMxX6LDO8kldNeu-DxQ"
       ,"https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcRAgKkpDyzk8kdIqk5ECsZ14XgbpBzyWFvrCrHombkSBAUn6jFo"
       ]
