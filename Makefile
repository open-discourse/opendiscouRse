
#define DB container url

CONTAINER ?= ghcr.io/open-discourse/open-discourse/database:1.2.0


# Start DB
start-db:
		docker pull $(CONTAINER)
		docker run --name="OD" --env POSTGRES_USER=postgres --env POSTGRES_DB=postgres --env POSTGRES_PASSWORD=postgres -p 5432:5432 -d $(CONTAINER)

delete-db: 
		docker stop OD
		docker rm OD