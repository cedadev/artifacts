echo copying globbed files 
cp /home/badc/badc/ujcc/webpage/plots/eafie* /home/badc/www/badc_site/htdocs/data/ujcc/eafie/.
echo cropping gifs
for file in eafie_*.gif
do
convert -crop 680x330+50+150 $file crop_$file
done
rm crop_eafie_gulf*
rm crop_eafie_atlantic*
rm crop_eafie_pacificSST_hovmol*
convert -crop 550x370+98+110 eafie_gulf.gif crop_eafie_gulf.gif
convert -crop 550x370+98+110 eafie_atlantic_precip.gif crop_eafie_atlantic_precip.gif
convert -crop 662x470+45+80 eafie_pacificSST_hovmol.gif crop_eafie_pacificSST_hovmol.gif

