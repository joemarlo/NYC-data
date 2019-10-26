curl -O -L https://s3.amazonaws.com/tripdata/[2013-2016][01-12]-citibike-tripdata.zip
curl -O -L https://s3.amazonaws.com/tripdata/[2017-2019][01-12]-citibike-tripdata.csv.zip
unzip '*.zip'
rm *.zip
