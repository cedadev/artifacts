echo copying globbed files 
cp /home/badc/badc/ujcc/webpage/plots/eacsa* /home/badc/www/badc_site/htdocs/data/ujcc/eacsa/.
echo cropping gifs
for file in eacsa_*.gif
do
convert -crop 680x330+50+150 $file crop_$file
done
rm crop_eacsa_gulf*
rm crop_eacsa_atlantic*
rm crop_eacsa_pacificSST_hovmol*
convert -crop 550x370+98+110 eacsa_gulf.gif crop_eacsa_gulf.gif
convert -crop 550x370+98+110 eacsa_atlantic_precip.gif crop_eacsa_atlantic_precip.gif
convert -crop 662x470+45+80 eacsa_pacificSST_hovmol.gif crop_eacsa_pacificSST_hovmol.gif

