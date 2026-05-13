FROM mcr.microsoft.com/dotnet/sdk:10.0 AS build

WORKDIR /src
COPY . .
RUN dotnet tool restore
RUN dotnet restore adult-fiction-catalog.fsproj
RUN dotnet publish adult-fiction-catalog.fsproj -c release -o /app

FROM mcr.microsoft.com/dotnet/runtime:10.0

RUN apt-get -y update && apt-get install -y --no-install-recommends ffmpeg python3 python3-pip
RUN python3 -m pip install --break-system-packages yt-dlp

COPY --from=build /app /app
USER ubuntu
WORKDIR /work

ENTRYPOINT ["/app/adult-fiction-catalog"]