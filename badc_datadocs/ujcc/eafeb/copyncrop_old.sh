echo copying globbed files 
cp /home/badc/badc/ujcc/webpage/plots/eafeb* /home/badc/www/badc_site/htdocs/data/ujcc/eafeb/.
echo cropping gifs
for file in eafeb_*.gif
do
convert -crop 680x330+50+150 $file crop_$file
done
rm crop_eafeb_gulf*
rm crop_eafeb_atlantic*
rm crop_eafeb_pacificSST_hovmol*
convert -crop 550x370+98+110 eafeb_gulf.gif crop_eafeb_gulf.gif
convert -crop 550x370+98+110 eafeb_atlantic_precip.gif crop_eafeb_atlantic_precip.gif
convert -crop 680x470+30+80 eafeb_pacificSST_hovmol.gif crop_eafeb_pacificSST_hovmol.gif

