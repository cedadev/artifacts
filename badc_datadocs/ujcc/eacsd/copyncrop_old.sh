echo copying globbed files 
cp /home/badc/badc/ujcc/webpage/plots/eacsd* /home/badc/www/badc_site/htdocs/data/ujcc/eacsd/.
echo cropping gifs
for file in eacsd_*.gif
do
convert -crop 680x330+50+150 $file crop_$file
done
rm crop_eacsd_gulf*
rm crop_eacsd_atlantic*
rm crop_eacsd_pacificSST_hovmol*
convert -crop 550x370+98+110 eacsd_gulf.gif crop_eacsd_gulf.gif
convert -crop 550x370+98+110 eacsd_atlantic_precip.gif crop_eacsd_atlantic_precip.gif
convert -crop 662x470+45+80 eacsd_pacificSST_hovmol.gif crop_eacsd_pacificSST_hovmol.gif

