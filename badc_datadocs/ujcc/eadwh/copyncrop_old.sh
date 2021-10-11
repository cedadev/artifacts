echo copying globbed files 
cp /home/badc/badc/ujcc/webpage/plots/eadwh* /home/badc/www/badc_site/htdocs/data/ujcc/eadwh/.
echo cropping gifs
for file in eadwh_*.gif
do
convert -crop 680x330+50+150 $file crop_$file
done
rm crop_eadwh_gulf*
rm crop_eadwh_atlantic*
rm crop_eadwh_pacificSST_hovmol*
convert -crop 550x370+98+110 eadwh_gulf.gif crop_eadwh_gulf.gif
convert -crop 550x370+98+110 eadwh_atlantic_precip.gif crop_eadwh_atlantic_precip.gif
convert -crop 662x470+45+80 eadwh_pacificSST_hovmol.gif crop_eadwh_pacificSST_hovmol.gif

