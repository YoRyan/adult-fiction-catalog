FROM mcr.microsoft.com/dotnet/sdk:8.0 AS build

WORKDIR /src
COPY *.fsproj *.fs .
RUN dotnet restore FMCollection.fsproj
RUN dotnet publish FMCollection.fsproj -c release -o /app --no-self-contained --no-restore

FROM mcr.microsoft.com/dotnet/runtime:8.0

RUN groupadd -r scrape && useradd --no-log-init -r -g scrape scrape
USER scrape
WORKDIR /app
COPY --from=build /app .

ENTRYPOINT ["./FMCollection"]