FROM mcr.microsoft.com/dotnet/sdk:8.0 AS build

WORKDIR /src
COPY . .
RUN dotnet restore PaysiteManager.fsproj
RUN dotnet publish PaysiteManager.fsproj -c release -o /app --no-self-contained --no-restore

FROM mcr.microsoft.com/dotnet/runtime:8.0

RUN apt-get -y update && apt-get install -y --no-install-recommends ffmpeg python3 python3-pip
RUN python3 -m pip install --break-system-packages yt-dlp

WORKDIR /app
COPY --from=build /app .

ENTRYPOINT ["./PaysiteManager"]