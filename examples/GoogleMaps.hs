
main= do
 setHeader $ do
    title $ "Accessing arguments in UI events"
    meta ! name="viewport" ! content "initial-scale=1.0, user-scalable=no"
    meta ! charset="utf-8"
    style "\
      \html, body, #map-canvas {\
        \height: 100%;\
        \margin: 0px;\
        \padding: 0px"
   
    script ! src "https://maps.googleapis.com/maps/api/js?v=3.exp"

 runBody $ do

  let initialize=
     mapOptions = {
       zoom: 4,
       center: new google.maps.LatLng(-25.363882,131.044922)
     };
  var map = new google.maps.Map(document.getElementById('map-canvas'),
      mapOptions);

  google.maps.event.addListener(map, 'click', function(e) {
    placeMarker(e.latLng, map);
  });
}

function placeMarker(position, map) {
  var marker = new google.maps.Marker({
    position: position,
    map: map
  });
  map.panTo(position);
}

google.maps.event.addDomListener(window, 'load', initialize);

    </script>
  </head>
  <body>
    <div id="map-canvas"></div>
  </body>