
SELECT cd_opecont, cd_taxon, cd_OMNIDIA, comptage, cd_site, date_opecont, x, y, commune FROM compil
join pandore.site using (cd_site)
join pandore.listes_diatomee using (cd_opecont) WHERE date_opecont BETWEEN '2007-01-01' AND '2021-12-31'
