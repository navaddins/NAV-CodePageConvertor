USE [YourDBName]
GO

DECLARE @RC int
DECLARE @verbose bit = 0 -- 0 is debug mode, 1 is execute the command
DECLARE @disableNonClustered bit = 1
DECLARE @skipEmptyTable bit = 1
DECLARE @companyName nvarchar(50) = N'' -- blank for all companies
DECLARE @removeCrLf bit = 1 -- 0 does not remove crlf, 1 remove crlf
DECLARE @codepagefromName nvarchar(50) = N'Windows_1252'
DECLARE @codepagetoName nvarchar(50) = N'big5'
DECLARE @maxDOP tinyint = 0

-- TODO: Set parameter values here.
EXECUTE @RC = [dbo].[ConvertCodePage] 
	@verbose
	,@disableNonClustered
	,@skipEmptyTable
	,@companyName
	,@removeCrLf
	,@codepagefromName
	,@codepagetoName
	,@maxDOP
GO