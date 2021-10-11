echo copying globbed files 
cp /home/badc/badc/higem/webpage/plots/xbylk* /home/badc/www/badc_site/htdocs/data/higem/xbylk/.
echo cropping gifs
for file in xbylk_*.gif
do
convert -crop 680x330+50+150 $file crop_$file
done
rm crop_xbylk_gulf*
rm crop_xbylk_atlantic*
rm crop_xbylk_pacificSST_hovmol*
convert -crop 550x370+98+110 xbylk_gulf.gif crop_xbylk_gulf.gif
convert -crop 550x370+98+110 xbylk_atlantic_precip.gif crop_xbylk_atlantic_precip.gif
convert -crop 662x470+45+80 xbylk_pacificSST_hovmol.gif crop_xbylk_pacificSST_hovmol.gif

