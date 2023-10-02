## new values fot number of trips that can be replaced from car to Bike + PT (instead of total trops old+new)

TESTEbug1 = CAR_TP_TRANSF %>%
  group_by(LTS, mode_TP) %>% 
  summarise(TP_4 = sum(Bike_new4), TP_10 = sum(Bike_new10)) %>% 
  ungroup() |> 
  pivot_longer(c(3,4), names_to = "ENMAC", values_to = "viagens") %>% 
  pivot_wider(names_from = mode_TP, values_from = viagens) %>% 
  mutate (Total = rowSums(.[3:6])) %>% 
  as.data.frame()

TESTEbug2 = CAR_TP_TRANSF %>%
  group_by(LTS, mode_TP) %>% 
  summarise(TP_4 = sum(TP_4), TP_10 = sum(TP_10)) %>% 
  ungroup() |> 
  pivot_longer(c(3,4), names_to = "ENMAC", values_to = "viagens") %>% 
  pivot_wider(names_from = mode_TP, values_from = viagens) %>% 
  mutate (Total = rowSums(.[3:6])) %>% 
  as.data.frame()

# none of them make sense

# What am I missing here?


# left_join the intermoddality routes with the pt segment

INTERMODALcar_check = routes_r5r_100jit_lts3__intermod_NoSub_elev |>
  filter(segment == 2) |> 
  left_join(routes_r5r_100jit_car_TP_lts3_ONLYTPsegment, by = "id")

table(INTERMODALcar_check$mode.x)
# BUS FERRY  RAIL  TRAM 
# 474   208 10871   605 

INTERMODALcar_check = routes_r5r_100jit_lts3__intermod_NoSub_elev |>
  filter(segment == 2) |> 
  left_join(routes_r5r_100jit_car_TP_lts3_ONLYTPsegment, by = "id") |> 
  filter(!is.na(mode.y))

# ok, já entendi. nós não calculámos a rota em automóvel só da segunda perna. isto porque a rota do início ao fim pode ser completamente diferente da rota 1 + 2+ 3.

# Mas calculámo sim
# ver routes_allmodesNSub3_TPonly_preoverline
são 5731 rotas da leg em TP

TESTEbutTP3 = routes_allmodesNSub3_TPonly_preoverline |> 
  group_by(mode) |> 
  summarise(Bike = sum(Bike),
            Car = sum(Car + CarP),
            TP = sum(Other),
            Total = sum(Total),
            new_cyc4 = sum(new_cyc4),
            new_cyc10 = sum(new_cyc10))
table(routes_allmodesNSub3_TPonly_preoverline$mode)


# > 3589+573
# [1] 4162
# > 4882+285
# [1] 5167
# > sum(TESTEbutTP3$Bike)
# [1] 2311.753
# > sum(TESTEbutTP3$new_cyc4)
# [1] 20385.23
# > sum(TESTEbutTP3$new_cyc4)*2
# [1] 40770.45
# > sum(TESTEbutTP3$Total)
# [1] 538514
# > sum(TESTEbutTP3$Total)*2
# [1] 1077028

# O erro está aqui. Duplicámos o número de viagens em bicicleta (2 legs!) quando calculámos o potential cycling para a tabela 1 do paper

As estimativas SE foram mal calculadas. a tabela HEAT_aml_results mostra que estão o dobro do base e potencial de viagens. A soma das distâncias estará correta,
pois terá sido feito um sum em todos.

Este erro será apenas referente ao intermodal, para a parte da bicicleta.
Isto deve-se a ter deixado as rotas separadas - o que faz todo o sentido - e depois ter pegado nesse ficheiro para o heat.




### Por enquanto refazer a tabela 2. Depois tentar calcular de novo as emissões


summary23_pre = routes_allmodesNSub3_TPonly_preoverline |> 
  st_drop_geometry() |> 
  group_by(mode) |> 
  summarise(Bike = sum(Bike),
            Car = sum(Car + CarP),
            TP = sum(Other),
            Total = sum(Total),
            new_cyc4 = sum(new_cyc4),
            new_cyc10 = sum(new_cyc10)) |> 
  ungroup()

summary23 = data.frame(
  Target = c("4%", "10%"),
  Routing = "safe", #3
  Baseline = sum(summary23_pre$Bike),
  Potential = c(sum(summary23_pre$new_cyc4), sum(summary23_pre$new_cyc10)),
  Bus = c(summary23_pre[[1,6]], summary23_pre[[1,7]]),
  Ferry = c(summary23_pre[[2,6]], summary23_pre[[2,7]]),
  Train = c(summary23_pre[[3,6]], summary23_pre[[3,7]]),
  Tram = c(summary23_pre[[4,6]], summary23_pre[[4,7]])
)


  
summary24_pre = routes_allmodesNSub4_TPonly_preoverline |> 
  st_drop_geometry() |> 
  group_by(mode) |> 
  summarise(Bike = sum(Bike),
            Car = sum(Car + CarP),
            TP = sum(Other),
            Total = sum(Total),
            new_cyc4 = sum(new_cyc4),
            new_cyc10 = sum(new_cyc10)) |> 
  ungroup()

summary24 = data.frame(
  Target = c("4%", "10%"),
  Routing = "direct", #4
  Baseline = sum(summary24_pre$Bike),
  Potential = c(sum(summary24_pre$new_cyc4), sum(summary24_pre$new_cyc10)),
  Bus = c(summary24_pre[[1,6]], summary24_pre[[1,7]]),
  Ferry = c(summary24_pre[[2,6]], summary24_pre[[2,7]]),
  Train = c(summary24_pre[[3,6]], summary24_pre[[3,7]]),
  Tram = c(summary24_pre[[4,6]], summary24_pre[[4,7]])
)

summary2 = rbind(summary23, summary24) |> 
  mutate(across(where(is.numeric), ~round(.)))

saveRDS(summary2, "paper/PaperTRA/load/summary2.Rds")

## Tenho de corrigir isto a montante, não apenas para o paper.


