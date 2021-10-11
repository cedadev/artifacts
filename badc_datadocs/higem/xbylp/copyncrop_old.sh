echo copying globbed files 
cp /home/badc/badc/higem/webpage/plots/xbylp* /home/badc/www/badc_site/htdocs/data/higem/xbylp/.
echo cropping gifs
for file in xbylp_*.gif
do
convert -crop 680x330+50+150 $file crop_$file
done
rm crop_xbylp_gulf*
rm crop_xbylp_atlantic*
rm crop_xbylp_pacificSST_hovmol*
convert -crop 550x370+98+110 xbylp_gulf.gif crop_xbylp_gulf.gif
convert -crop 550x370+98+110 xbylp_atlantic_precip.gif crop_xbylp_atlantic_precip.gif
convert -crop 662x470+45+80 xbylp_pacificSST_hovmol.gif crop_xbylp_pacificSST_hovmol.gif

