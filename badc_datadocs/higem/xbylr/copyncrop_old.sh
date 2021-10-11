echo copying globbed files 
cp /home/badc/badc/higem/webpage/plots/xbylr* /home/badc/www/badc_site/htdocs/data/higem/xbylr/.
echo cropping gifs
for file in xbylr_*.gif
do
convert -crop 680x330+50+150 $file crop_$file
done
rm crop_xbylr_gulf*
rm crop_xbylr_atlantic*
rm crop_xbylr_pacificSST_hovmol*
convert -crop 550x370+98+110 xbylr_gulf.gif crop_xbylr_gulf.gif
convert -crop 550x370+98+110 xbylr_atlantic_precip.gif crop_xbylr_atlantic_precip.gif
convert -crop 662x470+45+80 xbylr_pacificSST_hovmol.gif crop_xbylr_pacificSST_hovmol.gif

