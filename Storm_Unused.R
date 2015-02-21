
# Unused StormCOde


par(pch=01)
plot( mergedpd$year_group, mergedpd$propdmg / 1000, main="Property Damage by Year",
      axes=F, pch=9,
      xlab = "Year", ylab = "$$ in Billions")
lines(mergedpd$pd_cpi / 1000,mergedpd$Year,col="green")
axis(2)
axis(1, at= seq(1,62, by=5), labels=yrlabels)
box()


# -----------------



plot(sum_inj_yr$injuries ~ gev_sum$year_group)

plot(sum_fat_yr$fatalities, sum_fat_yr$year_group,
     main = "Fatalities per Year",
     axes=F, pch=9,
     xlab = "Year", ylab = "Fatalities")
axis(2)
axis(1, at= seq(1,62, by=5), labels=yrlabels)
box()


plot( sum_inj_yr$year, sum_inj_yr$injuries, 
      axes=F,
      main="Injuries per year",
      xlab = "Year", ylab = "Number of Injuries")

axis(1, at= seq(1,62, by=5), labels=yrlabels)
axis(2)
box()

par(pch=01)
plot(sum_propdmg_yr$year, sum_propdmg_yr$propdmg / 1000,  main="Property Damage by Year",
     axes=F, pch=9,
     xlab = "Year", ylab = "$$ in Billions")
axis(2)
axis(1, at= seq(1,62, by=5), labels=yrlabels)
box()



boxplot(sum_fat_yr$fatalities, main="Fatalities")
boxplot(sum_inj_yr$injuries, main="Injuries")
boxplot(sum_propdmg_yr$propdmg, main="Property Damage")
