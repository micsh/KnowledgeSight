# Install knowledge-sight — downloads the latest release binary and adds it to PATH.
# Usage: irm https://raw.githubusercontent.com/micsh/KnowledgeSight/main/install.ps1 | iex

$ErrorActionPreference = "Stop"

$Repo = "micsh/KnowledgeSight"
$Tool = "knowledge-sight"
$Rid = "win-x64"
$InstallDir = "$HOME\.knowledge-sight"

# Get latest release tag
$Release = Invoke-RestMethod "https://api.github.com/repos/$Repo/releases/latest"
$Tag = $Release.tag_name
Write-Host "Installing $Tool $Tag for $Rid..."

# Download and extract
$Archive = "$Tool-$Rid.zip"
$Url = "https://github.com/$Repo/releases/download/$Tag/$Archive"
$TempZip = Join-Path $env:TEMP $Archive

Invoke-WebRequest -Uri $Url -OutFile $TempZip
New-Item -ItemType Directory -Path $InstallDir -Force | Out-Null
Expand-Archive -Path $TempZip -DestinationPath $InstallDir -Force
Remove-Item $TempZip -Force

# Add to PATH if not already there
$UserPath = [Environment]::GetEnvironmentVariable("Path", "User")
if ($UserPath -notlike "*$InstallDir*") {
    [Environment]::SetEnvironmentVariable("Path", "$InstallDir;$UserPath", "User")
    Write-Host "Added $InstallDir to user PATH — restart your terminal to use it."
}

Write-Host "`n✓ Installed $Tool $Tag to $InstallDir"
Write-Host "  Run: $Tool --help"
