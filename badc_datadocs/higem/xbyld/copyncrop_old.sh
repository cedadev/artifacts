echo copying globbed files 
cp /home/badc/badc/higem/webpage/plots/xbyld* /home/badc/www/badc_site/htdocs/data/higem/xbyld/.
echo cropping gifs
for file in xbyld_*.gif
do
convert -crop 680x330+50+150 $file crop_$file
done
rm crop_xbyld_gulf*
rm crop_xbyld_atlantic*
rm crop_xbyld_pacificSST_hovmol*
convert -crop 550x370+98+110 xbyld_gulf.gif crop_xbyld_gulf.gif
convert -crop 550x370+98+110 xbyld_atlantic_precip.gif crop_xbyld_atlantic_precip.gif
convert -crop 662x470+45+80 xbyld_pacificSST_hovmol.gif crop_xbyld_pacificSST_hovmol.gif

