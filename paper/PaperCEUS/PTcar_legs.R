# see heat-prep_car.R to see where these data comes from

carlegs_2_or = OD_2legs_lts3 |> st_as_sf(sf_column_name = "origins")
carlegs_2_de = OD_2legs_lts3 |> st_as_sf(sf_column_name = "destinations")

# mapview::mapview(carlegs_2)
# id = 5123

carlegs_2_od = st_coordinates(OD_2legs_lts3 |> st_as_sf(sf_column_name = "origins")) |> as.data.frame() |> 
  cbind(st_coordinates(OD_2legs_lts3 |> st_as_sf(sf_column_name = "destinations")) |> as.data.frame()) |> 
  cbind(OD_2legs_lts3$id)

carlegs_2_od = stplanr::od_coords2line(carlegs_2_od)

mapview::mapview(carlegs_2_od)
id_route = 45563


###### LEG 2 in PT
mapview::mapview(carlegs_2_od[669,])



carlegs_13_od = OD_13legs_lts3

###### LEG 2 in Bike
mapview::mapview(carlegs_13_od[carlegs_13_od$id_route == 45563,])





# Try to run the routing model WITH grometry to get car routing

mapview::mapview(carlegs_13_od[carlegs_13_od$id_route == 45563,], color = "red") + mapview::mapview(carlegs_2_od[669,])




