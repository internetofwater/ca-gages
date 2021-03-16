# pygeoapi configuration

The geoconnex reference data server is a work in progress based on the [pygeoapi project.](https://pygeoapi.io/)

The server configuration can be found in [pygeoapi.config.yml](pygeoapi.config.yml)

Data hosted by the server can be found in [data](data)

## Contributing new content

Three pieces of information are required for a new contribution:  
1. spatial feature geometry and attributes in [data](data)
1. a pygeoapi resource configuration in [pygeoapi.config.yml](pygeoapi.config.yml)
1. PIDs registered with the `geoconnex.us` pid server for the features

### Spatial Features
Spatial feature data should be contributed in `SQLite GeoPackage` (preferably) or `geojson` format and be optimized for simple web-preview. This means the geometry should be simplified as much as is practical and attributes should be useful to a general audience.

At a minimum, the features should include attributes containing a name for the features and the PIDs of the features. The PIDs should be in an attribute titled `uri`. 

### pygeoapi configuration

See existing datasets for sample configuration. The configuration should include some JSON context configuration that associate attributes of the data to JSON-ld properties. This might look like:

```
        context:
            - name: https://schema.org/name
            - url: https://schema.org/subjectOf
            - description: https://schema.org/description
            - uri: "@id"
```

The `uri` element is required and ensures that the attributes get associated with the feature's PID rather than the URL of the https://info.geoconnex.us reference data server.

Many other context elements are possible. The [ELFIE project](https://opengeospatial.github.io/ELFIE/) has focused on that topic and can be a source of inspiration.

It is expected that the attributes and richness of these contexts will expand over time but getting some basic content in the system is better than nothing, so please don't hesitate to get something started and open a pull request. The geoconnex crew is more than happy to help get things across the finish line!

### PIDs for features

The features hosed in the Linked Data Server are intended to provide landing pages for PIDs

These reference features are intended to be used by the Stream Planning Tool and associated SB 19 data products

## Install

```
svn checkout https://github.com/internetofwater/CA-Gages/trunk/Linked Data Server
cd "Linked Data Server"
docker-compose up -d
```
