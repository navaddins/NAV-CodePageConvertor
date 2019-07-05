-- ================================================
-- Template generated from Template Explorer using:
-- Create Procedure (New Menu).SQL
--
-- This block of comments will not be included in
-- the definition of the procedure.
-- ================================================
SET ANSI_NULLS ON
SET QUOTED_IDENTIFIER ON
GO

IF (OBJECT_ID('CommandExecute') IS NOT NULL)
    DROP PROCEDURE CommandExecute
GO
-- =============================================
-- Author:    	NavAddIns
-- Create date: 2018-08-08
-- Description:	CommandExecute
-- =============================================
CREATE PROCEDURE [dbo].[CommandExecute] @varDBName nvarchar(250),
@varSQL nvarchar(max),
@varUseBeginCommit bit = 1,
@varResult bit = 0 OUTPUT
AS
BEGIN
    SET NOCOUNT ON;
    SET ANSI_PADDING ON;
    SET ANSI_WARNINGS ON;
    SET ARITHABORT ON;
    SET CONCAT_NULL_YIELDS_NULL ON;
    SET QUOTED_IDENTIFIER ON;
    SET NUMERIC_ROUNDABORT OFF;
    SET LANGUAGE us_english;
    SET DATEFORMAT mdy;
    SET DATEFIRST 7;
    BEGIN TRY
		IF (@varUseBeginCommit = 1)
        	BEGIN TRANSACTION;
			
            IF (LEN(@varDBName) <> 0)
                EXECUTE (N'USE QUOTENAME(PARSENAME(' + @varDBName + ',1)); EXEC sp_executesql N' + @varSQL);
            ELSE
                EXEC sp_executesql @varSQL;
        
		IF (@varUseBeginCommit = 1)
			COMMIT TRANSACTION;
        SET @varResult = 1;
    END TRY
    BEGIN CATCH
        IF (@varUseBeginCommit = 1)
			IF (@@TRANCOUNT > 0)
				ROLLBACK TRANSACTION;
        THROW;
    END CATCH
END
    RETURN @varResult;
GO
-- ================================================
-- Template generated from Template Explorer using:
-- Create Procedure (New Menu).SQL
--
-- This block of comments will not be included in
-- the definition of the procedure.
-- ================================================
-- =============================================
-- Author:		NavAddIns
-- Create date: 2018-08-08
-- Description:	NAV CodePageConversion
-- =============================================
SET ANSI_NULLS ON
SET QUOTED_IDENTIFIER ON
GO

IF (OBJECT_ID('ConvertCodePage') IS NOT NULL)
    DROP PROCEDURE ConvertCodePage
GO

CREATE PROCEDURE [dbo].ConvertCodePage @verbose bit = 0,
@disableNonClustered bit = 1,
@skipEmptyTable bit = 1,
@companyName nvarchar(50) = N'',
@removeCrLf bit = 1,
@codepagefromName nvarchar(50) = N'',
@codepagetoName nvarchar(50) = N'',
@maxDOP tinyint = 0
AS
BEGIN
    SET NOCOUNT ON;
    SET ANSI_PADDING ON;
    SET ANSI_WARNINGS ON;
    SET ARITHABORT ON;
    SET CONCAT_NULL_YIELDS_NULL ON;
    SET NUMERIC_ROUNDABORT OFF;
    SET LANGUAGE us_english;
    SET DATEFORMAT mdy;
    SET DATEFIRST 7;

    DECLARE @company nvarchar(250) = N'';
    DECLARE @databaseversionno int = 0;
    DECLARE @databasecollation nvarchar(250) = N'';
    DECLARE @datafilegroup nvarchar(max) = N'';

    DECLARE @sql nvarchar(max) = N'';
    DECLARE @table nvarchar(250) = N'';
    DECLARE @columnList nvarchar(max) = N'';
    DECLARE @columnListNaked nvarchar(max) = N'';
    DECLARE @columnName nvarchar(100) = N'';
    DECLARE @columnType int;
    DECLARE @columnTypeName nvarchar(8) = N'';
    DECLARE @maxLength int;

    DECLARE @notinStart int = 2000000001; -- System Table Start
    DECLARE @notinEnd int = 2000000166; -- System Table End
    DECLARE @isFirstLoop bit = 1; -- Do not change this value

    DECLARE @dbName nvarchar(max) = DB_NAME();
    DECLARE @tableNo int = 0;
    DECLARE @tableName nvarchar(250) = N'';
    DECLARE @patternTableName nvarchar(30) = QUOTENAME(N'\.|\/|\[|\]|\"');
    DECLARE @replaceTableName nvarchar(10) = N'_';
    DECLARE @tableView nvarchar(250) = N'';
    DECLARE @tempTableName nvarchar(250) = N'';
    DECLARE @viewName nvarchar(250) = N'';
    DECLARE @indexName nvarchar(max) = N'';
    DECLARE @rowCount AS int = 0;
    DECLARE @Result bit = 0;

    DECLARE @codepagefrom int;
    DECLARE @codepageto int;

    --- This is hardcode do not change ---
    SET @columnTypeName = 'nvarchar'; -- Use lowercase
    DECLARE @StartDate datetime
    DECLARE @StartDatePerComp datetime
    DECLARE @EndDate datetime
    DECLARE @Duration nvarchar(20) = N'';
    DECLARE @compatibilityLevel int = 0;
    DECLARE @parallelInsert nvarchar(13) = N''
    DECLARE @PrintMsg nvarchar(max) = N''
    DECLARE @MsgString nvarchar(max) = N'This is an informational message only. No user action is required.<<%s>>'
    DECLARE @MsgPrevString nvarchar(max) = N'This is an informational message only. No user action is required.<<Step No. %d. is already done.>>'

    SELECT
        @databasecollation = [collation_name],
        @compatibilityLevel = [compatibility_level]
    FROM sys.databases
    WHERE sys.databases.[name] = @dbName;

    SELECT
        @datafilegroup = QUOTENAME([name])
    FROM sys.filegroups
    WHERE is_default = 1;

    SELECT
        @databaseversionno = [databaseversionno]
    FROM [dbo].[$ndo$dbproperty];

    IF @compatibilityLevel >= 130
        SET @parallelInsert = 'WITH(TABLOCK)' + SPACE(1);

    -- Print Info.
    RAISERROR ('', 0, 1) WITH NOWAIT;
    RAISERROR ('-- Source DBName :%s', 0, 1, @dbName) WITH NOWAIT;
    RAISERROR ('-- Data File Group :%s', 0, 1, @datafilegroup) WITH NOWAIT;
    RAISERROR ('-- Filtered Companay :%s', 0, 1, @companyName) WITH NOWAIT;
    RAISERROR ('-- Change code page :%s to %s', 0, 1, @codepagefromName, @codepagetoName) WITH NOWAIT;
    IF (@verbose = 1)
        RAISERROR ('-- Debugging mode :%s', 0, 1, 'False') WITH NOWAIT;
    ELSE
        RAISERROR ('-- Debugging mode :%s', 0, 1, 'True') WITH NOWAIT;

    IF (@disableNonClustered = 1)
        RAISERROR ('-- NonClustered is disabled.', 0, 1) WITH NOWAIT;

    IF (@skipEmptyTable = 1)
        RAISERROR ('-- Skip empty table.', 0, 1) WITH NOWAIT;

    RAISERROR ('-- SQL Version :%s', 0, 1, @@VERSION) WITH NOWAIT;
    RAISERROR ('-- Maximum Degree of Parallelism :%d', 0, 1, @maxDOP) WITH NOWAIT;
    IF (LEN(@parallelInsert) > 0)
        RAISERROR ('-- Data insert mode :%s', 0, 1, 'Parallel Insert') WITH NOWAIT;
    --

    IF ((LEN(@codepagefromName) = 0)
        OR (@codepagefromName IS NULL))
    BEGIN
        RAISERROR ('Code Page from cannot be empty.', 16, 1);
        RETURN;
    END;

    IF ((LEN(@codepagetoName) = 0)
        OR (@codepagetoName IS NULL))
    BEGIN
        RAISERROR ('Code Page to cannot be empty.', 16, 1);
        RETURN;
    END;

    IF OBJECT_ID('CommandExecute') IS NULL
    BEGIN
        RAISERROR ('CommandExecute Stored Procedure is missing.', 16, 1);
        RETURN;
    END;

    IF (@databaseversionno < 70340)
        RAISERROR ('-- You cannot run this script for database version %d', 0, 1, @databaseversionno);
    ELSE
    BEGIN
    BEGIN TRY
        --Disable the Auto Update Statistics
        IF (@verbose = 1)
            EXECUTE [dbo].[CommandExecute] N'',N'ALTER DATABASE CURRENT SET AUTO_UPDATE_STATISTICS OFF;',0,@Result OUTPUT;

        SET @StartDate = GETDATE()
        SET @PrintMsg = '-- Start Time :' + CONVERT(varchar(20), @StartDate, 120);
        RAISERROR (N'%s', 0, 1, @PrintMsg) WITH NOWAIT;

        SELECT
            @codepagefrom = [Value]
        FROM master.dbo.GetCodePage()
        WHERE Code = @codepagefromName
        SELECT
            @codepageto = [Value]
        FROM master.dbo.GetCodePage()
        WHERE Code = @codepagetoName

        IF (@verbose = 1)
            IF OBJECT_ID('[dbo].ConversionLog') IS NULL
            BEGIN
                CREATE TABLE [dbo].[ConversionLog] (
                    [TableNo] [int] NOT NULL,
                    [TableName] [nvarchar](250) NOT NULL,
                    [TotalRecord] [int] NULL,
                    [StepNo] [smallint] NOT NULL,
                    [ExecuteCmd] [nvarchar](MAX) NULL,
                    [ProcessedTime] [time] NOT NULL,
                    [IsLastStep] [bit] NOT NULL,
                    CONSTRAINT [PK_ConversionLog] PRIMARY KEY CLUSTERED
                    (
                    [TableName] ASC,
                    [StepNo] ASC
                    ) WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
                ) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
                ALTER TABLE [dbo].[ConversionLog] ADD CONSTRAINT [DF_ConversionLog_TableNo] DEFAULT ((0)) FOR [TableNo]
                ALTER TABLE [dbo].[ConversionLog] ADD CONSTRAINT [DF_ConversionLog_TotalRecord] DEFAULT ((0)) FOR [TotalRecord]
            END

        -- Use temp for combine the table lists
        IF OBJECT_ID('tempdb..#tmpObjects') IS NOT NULL
            DROP TABLE #tmpObjects
        SELECT
            Objs.[ID] AS [Object ID],
            Objs.[Name] AS [Object Name],
            [SysObjs_ Table Name] AS [Table Name],
            [SysObjs_ Record Count] AS [Record Count],
            [SysObjs_ Company Name] AS [Company Name] INTO #tmpObjects
        FROM (SELECT
            SUM(sPTN.[rows]) AS [SysObjs_ Record Count],
            (CASE
                WHEN (CHARINDEX('$', sOBJ.name) = 0) THEN ''
                ELSE (
                    CASE
                        WHEN (LEFT(sOBJ.name, CHARINDEX('$', sOBJ.name) - 1) <> '') THEN LEFT(sOBJ.name, CHARINDEX('$', sOBJ.name) - 1)
                        ELSE ''
                    END
                    )
            END) AS [SysObjs_ Company Name],
            (CASE
                WHEN (CHARINDEX('$', sOBJ.name) = 0) THEN sOBJ.name
                ELSE (
                    CASE
                        WHEN (LEFT(sOBJ.name, CHARINDEX('$', sOBJ.name) - 1) <> '') THEN RIGHT(sOBJ.name, LEN(sOBJ.name) - CHARINDEX('$', sOBJ.name))
                        ELSE sOBJ.name
                    END
                    )
            END) AS [SysObjs_ Name],
            sOBJ.name AS [SysObjs_ Table Name]
        FROM sys.objects AS sOBJ
        INNER JOIN sys.partitions AS sPTN
            ON sOBJ.object_id = sPTN.object_id
        WHERE sOBJ.type = 'U'
        AND sOBJ.is_ms_shipped = 0x0
        AND index_id < 2 -- 0:Heap, 1:Clustered
        GROUP BY sOBJ.name) AS [tmpSysObjects]
        INNER JOIN [Object] AS Objs
            ON [SysObjs_ Name] = master.[dbo].ReplaceString(Objs.[Name], @patternTableName, @replaceTableName, 2)
            AND [SysObjs_ Company Name] = master.[dbo].ReplaceString(Objs.[Company Name], @patternTableName, @replaceTableName, 2)
        WHERE Objs.[Type] = 0

        DECLARE cmp_cursor CURSOR FAST_FORWARD FOR
        SELECT
            [Name]
        FROM [dbo].Company
        WHERE CASE
            WHEN ((@companyName IS NULL) OR
                (LEN(@companyName) = 0)) THEN ''
            ELSE [Name]
        END =
             CASE
                 WHEN ((@companyName IS NULL) OR
                     (LEN(@companyName) = 0)) THEN ''
                 ELSE @companyName
             END
        ORDER BY [dbo].Company.[Name]
        OPEN cmp_cursor
        FETCH NEXT FROM cmp_cursor INTO @company
        WHILE (@@FETCH_STATUS <> -1)
        BEGIN
            SET @StartDatePerComp = GETDATE();
            SELECT
                @company = master.[dbo].ReplaceString(@company, @patternTableName, @replaceTableName, 2);
            DECLARE tbl_cursor CURSOR FAST_FORWARD FOR
            SELECT
                [Object ID],
                [Table Name],
                [Record Count]
            FROM #tmpObjects
            WHERE NOT ([Object ID] BETWEEN @notinStart AND @notinEnd)
            AND (
            [Record Count] >
                            CASE
                                WHEN (@skipEmptyTable = 1) THEN 0
                                ELSE -1
                            END
            )
            AND (
            ([Company Name] =
                             CASE
                                 WHEN (@isFirstLoop = 1) THEN ''
                                 ELSE @company
                             END
            )
            OR ([Company Name] = @company)
            )
            --AND [Object Name] = 'Sales Line'
            --AND [Object Name] like 'S%'
            --AND (([Object ID] BETWEEN @notinStart AND @notinEnd) OR ([Object ID] = 0)) -- System Table Only
            --AND [Table Name] NOT LIKE CONCAT(@company,'$%') -- System Table and Global Table
            ORDER BY [Company Name], [Object Name]
            OPEN tbl_cursor
            FETCH NEXT FROM tbl_cursor INTO @tableNo, @table, @rowCount
            WHILE (@@FETCH_STATUS <> -1)
            BEGIN
                RAISERROR ('', 0, 1) WITH NOWAIT
                SET @isFirstLoop = 0;
                SET @tableName = @table;
                SET @tableView = CONCAT(@tableName, '$');
                SET @tempTableName = CONCAT(@tableView, 'temporary');

                DECLARE @hasIdentity int = 0;
                DECLARE @hasTable bit = 0;
                SELECT
                    @hasTable = COUNT(*)
                FROM sys.tables
                WHERE sys.tables.[name] = PARSENAME(@tableName, 1);
                IF (@hasTable = 0)
                BEGIN
                    SET @PrintMsg = '-- Table' + SPACE(1) + QUOTENAME(@tableName) + SPACE(1) + 'does not exist in' + SPACE(1) + DB_NAME();
                    RAISERROR (@MsgString, 0, 1, @PrintMsg) WITH NOWAIT
                END
                ELSE
                BEGIN
                    DECLARE @isProcessing AS int = 0;
                    IF (@verbose = 1)
                        SELECT
                            @isProcessing = COUNT(*)
                        FROM [dbo].ConversionLog
                        WHERE [dbo].ConversionLog.[TableName] = QUOTENAME(@tableName);

                    IF ((@rowCount = 0)
                        AND (@isProcessing = 0))
                    BEGIN
                        SET @PrintMsg = '-- Table' + SPACE(1) + QUOTENAME(@tableName) + SPACE(1) + 'record is zero. Nothing to do for' + SPACE(1) + QUOTENAME(@tableName);
                        RAISERROR (@MsgString, 0, 1, @PrintMsg) WITH NOWAIT
                    END
                    ELSE
                    BEGIN
                        DECLARE @stepNo AS smallint = 0;
                        DECLARE @isLastStep AS bit = 0;

                        SET @tableName = QUOTENAME(@tableName);
                        SET @tempTableName = QUOTENAME(@tempTableName);

                        IF (@verbose = 1)
                            SELECT TOP 1
                                @isLastStep = [IsLastStep]
                            FROM [dbo].ConversionLog
                            WHERE [dbo].ConversionLog.[TableName] = @tableName
                            AND [dbo].ConversionLog.[IsLastStep] = 1;

                        IF ((@verbose = 1)
                            AND (@isLastStep = 1))
                        BEGIN
                            SET @PrintMsg = '--' + SPACE(1) + @tableName + SPACE(1) + 'data conversion is already completed. Nothing to do for this table'
                            RAISERROR (@MsgString, 0, 1, @PrintMsg) WITH NOWAIT
                        END
                        ELSE
                        BEGIN

                            SET @PrintMsg = '-- Total Records (' + CAST(@rowCount AS nvarchar(max)) + ') of' + SPACE(1) + @tableName;
                            RAISERROR (N'%s', 0, 1, @PrintMsg) WITH NOWAIT;
                            -- Create column list
                            SET @columnList = N'';
                            SET @columnListNaked = '';
                            DECLARE col_cursor CURSOR FAST_FORWARD FOR
                            SELECT
                                [name],
                                [system_type_id],
                                [max_length]
                            FROM sys.columns
                            WHERE sys.columns.[object_id] = OBJECT_ID(PARSENAME(@tableName, 1))
                            AND sys.columns.[system_type_id] <> 189 -- Leave out timestamp column
                            OPEN col_cursor
                            FETCH NEXT FROM col_cursor INTO @columnName, @columnType, @maxLength;
                            WHILE (@@FETCH_STATUS <> -1)
                            BEGIN
                                SET @columnName = QUOTENAME(@columnName);
                                IF (@columnType = TYPE_ID(@columnTypeName))
                                BEGIN
                                    SET @columnList = @columnList + 'master.[dbo].CodePageConvertor(' + @columnName + ',' + CAST(@removeCrLf AS varchar(1)) + ','
                                    + CAST(@codepagefrom AS varchar(10)) + ',' + CAST(@codepageto AS nvarchar(30)) + ',' + '1' + ')'
                                    + SPACE(1) + 'COLLATE' + SPACE(1) + @databasecollation + SPACE(1) + 'AS' + SPACE(1) + @columnName + ','
                                END
                                ELSE
                                BEGIN
                                    SET @columnList = @columnList + @columnName + ',' + SPACE(1);
                                END
                                SET @columnListNaked = @columnListNaked + @columnName + ',' + SPACE(1);

                                FETCH NEXT FROM col_cursor INTO @columnName, @columnType, @maxLength;
                            END
                            CLOSE col_cursor
                            DEALLOCATE col_cursor

                            IF (LEN(@columnList) = 0)
                            BEGIN
                                SET @PrintMsg = '-- Nothing to do for' + SPACE(1) + @tableName;
                                RAISERROR (N'%s', 0, 1, @PrintMsg) WITH NOWAIT;
                                CONTINUE
                            END

                            SET @columnList = SUBSTRING(@columnList, 1, LEN(@columnList) - 1);
                            SET @columnListNaked = SUBSTRING(@columnListNaked, 1, LEN(@columnListNaked) - 1);

                            SET @sql = 'IF OBJECT_ID(''[dbo].' + @tempTableName + ''') IS NOT NULL DROP TABLE [dbo].' + @tempTableName + ';';

                            RAISERROR ('', 0, 1) WITH NOWAIT;
                            SET @PrintMsg = @sql;
                            RAISERROR (N'%s', 0, 1, @PrintMsg) WITH NOWAIT;

                            IF (@verbose = 1)
                            BEGIN
                                SET @stepNo = @stepNo + 1;
                                SELECT
                                    @isLastStep = COUNT(1)
                                FROM [dbo].ConversionLog
                                WHERE [dbo].ConversionLog.[TableName] = @tableName
                                AND [dbo].ConversionLog.[StepNo] = @stepNo;
                                IF (@isLastStep = 0)
                                BEGIN
                                    EXECUTE [dbo].[CommandExecute] N'',
                                                                   @sql,
                                                                   1,@Result OUTPUT;
									SET @sql = CONCAT('INSERT INTO [dbo].ConversionLog(TableName, StepNo, ExecuteCmd, ProcessedTime, IsLastStep)',SPACE(1),
													  'VALUES',SPACE(1),'(','''',@tableName,'''',',',@stepNo,',','''',REPLACE(@sql,'''',''''''),'''',',','''',GETDATE(),'''',',',0,');')
                                    IF (@Result <> 0)
                                        EXECUTE [dbo].[CommandExecute] N'',
                                                                       @sql,
                                                                       1,@Result OUTPUT;
                                --IF (@Result <> 0)
                                --	INSERT [dbo].ConversionLog (TableNo, TableName, TotalRecord, StepNo, ExecuteCmd, ProcessedTime, IsLastStep)
                                --		VALUES (@tableNo, @tableName, @rowCount, @stepNo, @sql, GETDATE(), 0);
                                END
                                ELSE
                                    RAISERROR (@MsgPrevString, 0, 1, @stepNo) WITH NOWAIT
                            END
                            SET @hasIdentity = (SELECT
                                COUNT(*)
                            FROM sys.columns
                            WHERE sys.columns.[is_identity] = 1
                            AND sys.columns.[object_id] = OBJECT_ID(PARSENAME(@tableName, 1)));

                            SET @sql = 'SET STATISTICS TIME ON;' + SPACE(1) + CHAR(10)
                            + 'SELECT' + SPACE(1) + @columnList + SPACE(1) + CHAR(10) + 'INTO [dbo].' + @tempTableName + SPACE(1) + 'FROM [dbo].' + @tableName + SPACE(1)
                            + 'OPTION' + SPACE(1) + '(MAXDOP' + SPACE(1) + CAST(@maxDOP AS nvarchar(1)) + ');' + SPACE(1) + CHAR(10)
                            + 'SET STATISTICS TIME OFF;';

                            SET @PrintMsg = @sql;
                            RAISERROR (N'%s', 0, 1, @PrintMsg) WITH NOWAIT;
                            --WAITFOR DELAY '00:00:01'
                            IF (@verbose = 1)
                            BEGIN
                                SET @stepNo = @stepNo + 1;
                                SELECT
                                    @isLastStep = COUNT(1)
                                FROM [dbo].ConversionLog
                                WHERE [dbo].ConversionLog.[TableName] = @tableName
                                AND [dbo].ConversionLog.[StepNo] = @stepNo;

                                IF (@isLastStep = 0)
                                BEGIN
                                    EXECUTE [dbo].[CommandExecute] N'',
                                                                   @sql,
                                                                   1,@Result OUTPUT;
									SET @sql = CONCAT('INSERT INTO [dbo].ConversionLog(TableName, StepNo, ExecuteCmd, ProcessedTime, IsLastStep)',SPACE(1),
													  'VALUES',SPACE(1),'(','''',@tableName,'''',',',@stepNo,',','''',REPLACE(@sql,'''',''''''),'''',',','''',GETDATE(),'''',',',0,');')
                                    IF (@Result <> 0)
                                        EXECUTE [dbo].[CommandExecute] N'',
                                                                       @sql,
                                                                       1,@Result OUTPUT;
                                    --IF (@Result <> 0)
                                    --	INSERT [dbo].ConversionLog (TableName, StepNo, ExecuteCmd, ProcessedTime, IsLastStep)
                                    --		VALUES (@tableName, @stepNo, @sql, GETDATE(), 0);
                                    RAISERROR ('', 0, 1) WITH NOWAIT;
                                END
                                ELSE
                                    RAISERROR (@MsgPrevString, 0, 1, @stepNo) WITH NOWAIT
                            END
                            -- Create the key on temp table
                            DECLARE @sql_tbl_Index nvarchar(max) = N'';
                            DECLARE @tbl_keyName nvarchar(100) = N'';
                            DECLARE @tbl_keyType int;
                            DECLARE @tbl_keyId int;

                            DECLARE @tbl_columnName nvarchar(max) = N'';
                            DECLARE @tbl_columnSorting int;
                            DECLARE @tbl_columnList nvarchar(max) = N'';
                            DECLARE @tbl_clustered nvarchar(100) = N'';

                            SET @sql_tbl_Index = N'';
                            DECLARE tbl_key_cursor CURSOR FAST_FORWARD FOR
                            SELECT
                                [name],
                                [type],
                                [index_id]
                            FROM sys.indexes
                            WHERE sys.indexes.[object_id] = OBJECT_ID(PARSENAME(@tableName, 1))
                            AND sys.indexes.[type] = 1;
                            OPEN tbl_key_cursor
                            FETCH NEXT FROM tbl_key_cursor INTO @tbl_keyName, @tbl_keyType, @tbl_keyId;
                            WHILE (@@FETCH_STATUS <> -1)
                            BEGIN
                                SET @tbl_clustered = 'CLUSTERED'
                                SET @tbl_columnList = N'';
                                DECLARE tbl_col_cursor CURSOR FAST_FORWARD FOR
                                SELECT
                                    [name],
                                    [is_descending_key]
                                FROM sys.index_columns ic
                                JOIN sys.columns c
                                    ON ic.object_id = c.object_id
                                WHERE c.object_id = OBJECT_ID(PARSENAME(@tableName, 1))
                                AND ic.column_id = c.column_id
                                AND index_id = @tbl_keyId
                                ORDER BY index_column_id;
                                OPEN tbl_col_cursor
                                FETCH NEXT FROM tbl_col_cursor INTO @tbl_columnName, @tbl_columnSorting;
                                WHILE (@@FETCH_STATUS <> -1)
                                BEGIN
                                    SET @tbl_columnList = @tbl_columnList + QUOTENAME(@tbl_columnName);
                                    IF @tbl_columnSorting = 0
                                        SET @tbl_columnList = @tbl_columnList + SPACE(1) + 'ASC,';
                                    ELSE
                                        SET @tbl_columnList = @tbl_columnList + SPACE(1) + 'DESC,';

                                    FETCH NEXT FROM tbl_col_cursor INTO @tbl_columnName, @tbl_columnSorting;
                                END
                                CLOSE tbl_col_cursor
                                DEALLOCATE tbl_col_cursor

                                SET @tbl_keyName = REPLACE(@tbl_keyName, PARSENAME(@tableName, 1), PARSENAME(@tempTableName, 1))
                                IF (LEN(@sql_tbl_Index) > 0)
                                    SET @sql_tbl_Index = @sql_tbl_Index + CHAR(10)

                                SET @sql_tbl_Index = @sql_tbl_Index + 'SET STATISTICS TIME ON;' + SPACE(1) + CHAR(10)
                                + 'CREATE UNIQUE' + SPACE(1) + @tbl_clustered + SPACE(1) + 'INDEX' + SPACE(1) + QUOTENAME(@tbl_keyName) + SPACE(1)
                                + 'ON [dbo].' + @tempTableName + SPACE(1) + '(' + SUBSTRING(@tbl_columnList, 1, LEN(@tbl_columnList) - 1) + ')' + SPACE(1) + CHAR(10)
                                + 'WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON' + SPACE(1)
                                + @datafilegroup + ';' + SPACE(1) + CHAR(10)
                                + 'SET STATISTICS TIME OFF;';

                                IF (LEN(@sql_tbl_Index) > 0)
                                BEGIN
                                    SET @sql = @sql_tbl_Index;
                                    SET @PrintMsg = @sql;
                                    RAISERROR (N'%s', 0, 1, @PrintMsg) WITH NOWAIT;
                                    IF (@verbose = 1)
                                    BEGIN
                                        SET @stepNo = @stepNo + 1;
                                        SELECT
                                            @isLastStep = COUNT(1)
                                        FROM [dbo].ConversionLog
                                        WHERE [dbo].ConversionLog.[TableName] = @tableName
                                        AND [dbo].ConversionLog.[StepNo] = @stepNo;

                                        IF (@isLastStep = 0)
                                        BEGIN
                                            EXECUTE [dbo].[CommandExecute] N'',
                                                                           @sql,
                                                                           1,@Result OUTPUT;
											SET @sql = CONCAT('INSERT INTO [dbo].ConversionLog(TableName, StepNo, ExecuteCmd, ProcessedTime, IsLastStep)',SPACE(1),
															  'VALUES',SPACE(1),'(','''',@tableName,'''',',',@stepNo,',','''',REPLACE(@sql,'''',''''''),'''',',','''',GETDATE(),'''',',',0,');')
                                            IF (@Result <> 0)
                                                EXECUTE [dbo].[CommandExecute] N'',
                                                                               @sql,
                                                                               1,@Result OUTPUT;
                                            --IF (@Result <> 0)
                                            --	INSERT [dbo].ConversionLog (TableName, StepNo, ExecuteCmd, ProcessedTime, IsLastStep)
                                            --		VALUES (@tableName, @stepNo, @sql, GETDATE(), 0);
                                            RAISERROR ('', 0, 1) WITH NOWAIT;
                                        END
                                        ELSE
                                            RAISERROR (@MsgPrevString, 0, 1, @stepNo) WITH NOWAIT
                                    END
                                END

                                FETCH NEXT FROM tbl_key_cursor INTO @tbl_keyName, @tbl_keyType, @tbl_keyId;
                            END
                            CLOSE tbl_key_cursor
                            DEALLOCATE tbl_key_cursor

                            -- Disable Index on view to avoide truncate error
                            DECLARE @sql_vw_Index nvarchar(max) = N'';
                            SET @sql_vw_Index = N'';
                            DECLARE vw_cursor CURSOR FAST_FORWARD FOR
                            SELECT
                                [name]
                            FROM sys.views
                            WHERE LEFT([name], LEN(@tableView)) = @tableView;
                            OPEN vw_cursor
                            FETCH NEXT FROM vw_cursor INTO @viewName;
                            WHILE (@@FETCH_STATUS <> -1)
                            BEGIN
                                SET @viewName = QUOTENAME(@viewName);

                                DECLARE @vw_keyName nvarchar(100) = N'';
                                DECLARE @vw_keyType int;
                                DECLARE @vw_keyId int;

                                DECLARE @vw_columnName nvarchar(max) = N'';
                                DECLARE @vw_columnSorting int;
                                DECLARE @vw_columnList nvarchar(max) = N'';
                                DECLARE @vw_clustered nvarchar(100) = N'';

                                DECLARE vw_key_cursor CURSOR FAST_FORWARD FOR
                                SELECT
                                    [name],
                                    [type],
                                    [index_id]
                                FROM sys.indexes
                                WHERE sys.indexes.[object_id] = OBJECT_ID(PARSENAME(@viewName, 1));
                                OPEN vw_key_cursor
                                FETCH NEXT FROM vw_key_cursor INTO @vw_keyName, @vw_keyType, @vw_keyId;
                                WHILE (@@FETCH_STATUS <> -1)
                                BEGIN
                                    -- generate create index command
                                    IF @vw_keyType = 1
                                    BEGIN
                                        SET @vw_clustered = 'CLUSTERED'
                                    END
                                    ELSE
                                    BEGIN
                                        SET @vw_clustered = 'NONCLUSTERED'
                                    END

                                    SET @vw_columnList = N'';
                                    DECLARE vw_col_cursor CURSOR FAST_FORWARD FOR
                                    SELECT
                                        [name],
                                        [is_descending_key]
                                    FROM sys.index_columns ic
                                    JOIN sys.columns c
                                        ON ic.object_id = c.object_id
                                    WHERE c.object_id = OBJECT_ID(PARSENAME(@viewName, 1))
                                    AND ic.column_id = c.column_id
                                    AND index_id = @vw_keyId
                                    ORDER BY index_column_id;
                                    OPEN vw_col_cursor
                                    FETCH NEXT FROM vw_col_cursor INTO @vw_columnName, @vw_columnSorting;
                                    WHILE (@@FETCH_STATUS <> -1)
                                    BEGIN
                                        SET @vw_columnList = @vw_columnList + QUOTENAME(@vw_columnName);
                                        IF @vw_columnSorting = 0
                                            SET @vw_columnList = @vw_columnList + SPACE(1) + 'ASC,';
                                        ELSE
                                            SET @vw_columnList = @vw_columnList + SPACE(1) + 'DESC,';

                                        FETCH NEXT FROM vw_col_cursor INTO @vw_columnName, @vw_columnSorting;
                                    END
                                    CLOSE vw_col_cursor
                                    DEALLOCATE vw_col_cursor

                                    IF (LEN(@sql_vw_Index) > 0)
                                        SET @sql_vw_Index = @sql_vw_Index + CHAR(10)

                                    SET @sql_vw_Index = @sql_vw_Index + 'CREATE UNIQUE' + SPACE(1) + @vw_clustered + SPACE(1) + 'INDEX' + SPACE(1) + QUOTENAME(@vw_keyName) + SPACE(1)
                                    + 'ON [dbo].' + @viewName + SPACE(1) + '(' + SUBSTRING(@vw_columnList, 1, LEN(@vw_columnList) - 1) + ')' + SPACE(1)
                                    + 'WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON' + SPACE(1)
                                    + @datafilegroup + ';';

                                    SET @sql = 'DROP INDEX' + SPACE(1) + QUOTENAME(@vw_keyName) + SPACE(1) + 'ON [dbo].' + @viewName + SPACE(1) + 'WITH ( ONLINE = OFF );';

                                    SET @PrintMsg = @sql;
                                    RAISERROR (N'%s', 0, 1, @PrintMsg) WITH NOWAIT;
                                    IF (@verbose = 1)
                                    BEGIN
                                        SET @stepNo = @stepNo + 1;
                                        SELECT
                                            @isLastStep = COUNT(1)
                                        FROM [dbo].ConversionLog
                                        WHERE [dbo].ConversionLog.[TableName] = @tableName
                                        AND [dbo].ConversionLog.[StepNo] = @stepNo;

                                        IF (@isLastStep = 0)
                                        BEGIN
                                            EXECUTE [dbo].[CommandExecute] N'',
                                                                           @sql,
                                                                           1,@Result OUTPUT;
											SET @sql = CONCAT('INSERT INTO [dbo].ConversionLog(TableName, StepNo, ExecuteCmd, ProcessedTime, IsLastStep)',SPACE(1),
															   'VALUES',SPACE(1),'(','''',@tableName,'''',',',@stepNo,',','''',REPLACE(@sql,'''',''''''),'''',',','''',GETDATE(),'''',',',0,');')
                                            IF (@Result <> 0)
                                                EXECUTE [dbo].[CommandExecute] N'',
                                                                               @sql,
                                                                               1,@Result OUTPUT;
                                        --IF (@Result <> 0)
                                        --	INSERT [dbo].ConversionLog (TableName, StepNo, ExecuteCmd, ProcessedTime, IsLastStep)
                                        --		VALUES (@tableName, @stepNo, @sql, GETDATE(), 0);
                                        END
                                        ELSE
                                            RAISERROR (@MsgPrevString, 0, 1, @stepNo) WITH NOWAIT
                                    END
                                    FETCH NEXT FROM vw_key_cursor INTO @vw_keyName, @vw_keyType, @vw_keyId;
                                END
                                CLOSE vw_key_cursor
                                DEALLOCATE vw_key_cursor
                                FETCH NEXT FROM vw_cursor INTO @viewName;
                            END
                            CLOSE vw_cursor
                            DEALLOCATE vw_cursor

                            -- Disable constraint all to avoide truncate error
                            SET @sql = 'ALTER TABLE' + SPACE(1) + @tableName + SPACE(1) + 'NOCHECK CONSTRAINT ALL;'
                            SET @PrintMsg = @sql;
                            RAISERROR (N'%s', 0, 1, @PrintMsg) WITH NOWAIT;
                            IF (@verbose = 1)
                            BEGIN
                                SET @stepNo = @stepNo + 1;
                                SELECT
                                    @isLastStep = COUNT(1)
                                FROM [dbo].ConversionLog
                                WHERE [dbo].ConversionLog.[TableName] = @tableName
                                AND [dbo].ConversionLog.[StepNo] = @stepNo;

                                IF (@isLastStep = 0)
                                BEGIN
                                    EXECUTE [dbo].[CommandExecute] N'',
                                                                   @sql,
                                                                   1,@Result OUTPUT;
									SET @sql = CONCAT('INSERT INTO [dbo].ConversionLog(TableName, StepNo, ExecuteCmd, ProcessedTime, IsLastStep)',SPACE(1),
													  'VALUES',SPACE(1),'(','''',@tableName,'''',',',@stepNo,',','''',REPLACE(@sql,'''',''''''),'''',',','''',GETDATE(),'''',',',0,');')
                                    IF (@Result <> 0)
                                        EXECUTE [dbo].[CommandExecute] N'',
                                                                       @sql,
                                                                       1,@Result OUTPUT;
                                --IF (@Result <> 0)
                                --	INSERT [dbo].ConversionLog (TableName, StepNo, ExecuteCmd, ProcessedTime, IsLastStep)
                                --		VALUES (@tableName, @stepNo, @sql, GETDATE(), 0);
                                END
                                ELSE
                                    RAISERROR (@MsgPrevString, 0, 1, @stepNo) WITH NOWAIT
                            END
                            --- Disable Cluster and NonCluster Index
                            SET @indexName = N''
                            IF (@disableNonClustered = 1)
                                SELECT
                                    @indexName = sysindex.[name]
                                FROM sys.indexes AS sysindex
                                INNER JOIN sys.tables AS systbl
                                    ON sysindex.[object_id] = systbl.object_id
                                WHERE systbl.[name] = PARSENAME(@tableName, 1)
                                AND (
                                sysindex.[type] = 1 -- Cluster Index
                                )
                                AND sysindex.[is_disabled] = 0

                            IF (LEN(@indexName) <> 0)
                            BEGIN
                                --- Disable Cluster and NonCluster Index
                                SET @sql = 'ALTER INDEX' + SPACE(1) + QUOTENAME(@indexName) + SPACE(1) + 'ON' + SPACE(1) + @tableName + SPACE(1) + 'DISABLE;'
                                SET @PrintMsg = @sql;
                                RAISERROR (N'%s', 0, 1, @PrintMsg) WITH NOWAIT;
                                IF (@verbose = 1)
                                BEGIN
                                    SET @stepNo = @stepNo + 1;
                                    SELECT
                                        @isLastStep = COUNT(1)
                                    FROM [dbo].ConversionLog
                                    WHERE [dbo].ConversionLog.[TableName] = @tableName
                                    AND [dbo].ConversionLog.[StepNo] = @stepNo;

                                    IF (@isLastStep = 0)
                                    BEGIN
                                        EXECUTE [dbo].[CommandExecute] N'',
                                                                       @sql,
                                                                       1,@Result OUTPUT;
									SET @sql = CONCAT('INSERT INTO [dbo].ConversionLog(TableName, StepNo, ExecuteCmd, ProcessedTime, IsLastStep)',SPACE(1),
													  'VALUES',SPACE(1),'(','''',@tableName,'''',',',@stepNo,',','''',REPLACE(@sql,'''',''''''),'''',',','''',GETDATE(),'''',',',0,');')
                                        IF (@Result <> 0)
                                            EXECUTE [dbo].[CommandExecute] N'',
                                                                           @sql,
                                                                           1,@Result OUTPUT;
                                    --IF (@Result <> 0)
                                    --	INSERT [dbo].ConversionLog (TableName, StepNo, ExecuteCmd, ProcessedTime, IsLastStep)
                                    --		VALUES (@tableName, @stepNo, @sql, GETDATE(), 0);
                                    END
                                    ELSE
                                        RAISERROR (@MsgPrevString, 0, 1, @stepNo) WITH NOWAIT
                                END
                                -- Clean out data in fromTable.
                                SET @sql = 'SET STATISTICS TIME ON;' + SPACE(1)
                                + 'TRUNCATE TABLE [dbo].' + @tableName + ';' + SPACE(1)
                                + 'SET STATISTICS TIME OFF;';
                                SET @PrintMsg = @sql;
                                RAISERROR (N'%s', 0, 1, @PrintMsg) WITH NOWAIT;
                                IF (@verbose = 1)
                                BEGIN
                                    SET @stepNo = @stepNo + 1;
                                    SELECT
                                        @isLastStep = COUNT(1)
                                    FROM [dbo].ConversionLog
                                    WHERE [dbo].ConversionLog.[TableName] = @tableName
                                    AND [dbo].ConversionLog.[StepNo] = @stepNo;

                                    IF (@isLastStep = 0)
                                    BEGIN
                                        EXECUTE [dbo].[CommandExecute] N'',
                                                                       @sql,
                                                                       1,@Result OUTPUT;
										SET @sql = CONCAT('INSERT INTO [dbo].ConversionLog(TableName, StepNo, ExecuteCmd, ProcessedTime, IsLastStep)',SPACE(1),
														  'VALUES',SPACE(1),'(','''',@tableName,'''',',',@stepNo,',','''',REPLACE(@sql,'''',''''''),'''',',','''',GETDATE(),'''',',',0,');')
                                        IF (@Result <> 0)
                                            EXECUTE [dbo].[CommandExecute] N'',
                                                                           @sql,
                                                                           1,@Result OUTPUT;
                                        --IF (@Result <> 0)
                                        --	INSERT [dbo].ConversionLog (TableName, StepNo, ExecuteCmd, ProcessedTime, IsLastStep)
                                        --		VALUES (@tableName, @stepNo, @sql, GETDATE(), 0);
                                        RAISERROR ('', 0, 1) WITH NOWAIT;
                                    END
                                    ELSE
                                        RAISERROR (@MsgPrevString, 0, 1, @stepNo) WITH NOWAIT
                                END

                                --- Enable back Cluster Index only
                                SET @sql = 'ALTER INDEX' + SPACE(1) + QUOTENAME(@indexName) + SPACE(1) + 'ON' + SPACE(1) + @tableName + SPACE(1) + 'REBUILD;'
                                SET @PrintMsg = @sql;
                                RAISERROR (N'%s', 0, 1, @PrintMsg) WITH NOWAIT;
                                IF (@verbose = 1)
                                BEGIN
                                    SET @stepNo = @stepNo + 1;
                                    SELECT
                                        @isLastStep = COUNT(1)
                                    FROM [dbo].ConversionLog
                                    WHERE [dbo].ConversionLog.[TableName] = @tableName
                                    AND [dbo].ConversionLog.[StepNo] = @stepNo;

                                    IF (@isLastStep = 0)
                                    BEGIN
                                        EXECUTE [dbo].[CommandExecute] N'',
                                                                       @sql,
                                                                       1,@Result OUTPUT;
										SET @sql = CONCAT('INSERT INTO [dbo].ConversionLog(TableName, StepNo, ExecuteCmd, ProcessedTime, IsLastStep)',SPACE(1),
														  'VALUES',SPACE(1),'(','''',@tableName,'''',',',@stepNo,',','''',REPLACE(@sql,'''',''''''),'''',',','''',GETDATE(),'''',',',0,');')
                                        IF (@Result <> 0)
                                            EXECUTE [dbo].[CommandExecute] N'',
                                                                           @sql,
                                                                           1,@Result OUTPUT;
                                    --IF (@Result <> 0)
                                    --	INSERT [dbo].ConversionLog (TableName, StepNo, ExecuteCmd, ProcessedTime, IsLastStep)
                                    --		VALUES (@tableName, @stepNo, @sql, GETDATE(), 0);
                                    END
                                    ELSE
                                        RAISERROR (@MsgPrevString, 0, 1, @stepNo) WITH NOWAIT
                                END
                            END
                            ELSE
                            BEGIN
                                -- Clean out data in fromTable.
                                SET @sql = 'SET STATISTICS TIME ON;' + SPACE(1)
                                + 'TRUNCATE TABLE [dbo].' + @tableName + ';' + SPACE(1)
                                + 'SET STATISTICS TIME OFF;';
                                SET @PrintMsg = @sql;
                                RAISERROR (N'%s', 0, 1, @PrintMsg) WITH NOWAIT;
                                IF (@verbose = 1)
                                BEGIN
                                    SET @stepNo = @stepNo + 1;
                                    SELECT
                                        @isLastStep = COUNT(1)
                                    FROM [dbo].ConversionLog
                                    WHERE [dbo].ConversionLog.[TableName] = @tableName
                                    AND [dbo].ConversionLog.[StepNo] = @stepNo;

                                    IF (@isLastStep = 0)
                                    BEGIN
                                        EXECUTE [dbo].[CommandExecute] N'',
                                                                       @sql,
                                                                       1,@Result OUTPUT;
										SET @sql = CONCAT('INSERT INTO [dbo].ConversionLog(TableName, StepNo, ExecuteCmd, ProcessedTime, IsLastStep)',SPACE(1),
											 			  'VALUES',SPACE(1),'(','''',@tableName,'''',',',@stepNo,',','''',REPLACE(@sql,'''',''''''),'''',',','''',GETDATE(),'''',',',0,');')
                                        IF (@Result <> 0)
                                            EXECUTE [dbo].[CommandExecute] N'',
                                                                           @sql,
                                                                           1,@Result OUTPUT;
                                        --IF (@Result <> 0)
                                        --	INSERT [dbo].ConversionLog (TableName, StepNo, ExecuteCmd, ProcessedTime, IsLastStep)
                                        --		VALUES (@tableName, @stepNo, @sql, GETDATE(), 0);
                                        RAISERROR ('', 0, 1) WITH NOWAIT;
                                    END
                                    ELSE
                                        RAISERROR (@MsgPrevString, 0, 1, @stepNo) WITH NOWAIT
                                END
                            END

                            IF (LEN(@sql_vw_Index) > 0)
                            BEGIN
                                SET @sql = @sql_vw_Index;
                                SET @PrintMsg = @sql;
                                RAISERROR (N'%s', 0, 1, @PrintMsg) WITH NOWAIT;
                                IF (@verbose = 1)
                                BEGIN
                                    SET @stepNo = @stepNo + 1;
                                    SELECT
                                        @isLastStep = COUNT(1)
                                    FROM [dbo].ConversionLog
                                    WHERE [dbo].ConversionLog.[TableName] = @tableName
                                    AND [dbo].ConversionLog.[StepNo] = @stepNo;

                                    IF (@isLastStep = 0)
                                    BEGIN
                                        EXECUTE [dbo].[CommandExecute] N'',
                                                                       @sql,
                                                                       1,@Result OUTPUT;
										SET @sql = CONCAT('INSERT INTO [dbo].ConversionLog(TableName, StepNo, ExecuteCmd, ProcessedTime, IsLastStep)',SPACE(1),
														  'VALUES',SPACE(1),'(','''',@tableName,'''',',',@stepNo,',','''',REPLACE(@sql,'''',''''''),'''',',','''',GETDATE(),'''',',',0,');')
                                        IF (@Result <> 0)
                                            EXECUTE [dbo].[CommandExecute] N'',
                                                                           @sql,
                                                                           1,@Result OUTPUT;
                                    --IF (@Result <> 0)
                                    --	INSERT [dbo].ConversionLog (TableName, StepNo, ExecuteCmd, ProcessedTime, IsLastStep)
                                    --		VALUES (@tableName, @stepNo, @sql, GETDATE(), 0);
                                    END
                                    ELSE
                                        RAISERROR (@MsgPrevString, 0, 1, @stepNo) WITH NOWAIT
                                END
                            END

                            IF (@hasIdentity > 0)
                            BEGIN
                                SET @sql = 'SET STATISTICS TIME ON;' + SPACE(1) + CHAR(10)
                                + 'SET IDENTITY_INSERT [dbo].' + @tableName + SPACE(1) + 'ON;' + SPACE(1) + CHAR(10)
                                + 'INSERT INTO [dbo].' + @tableName + SPACE(1) + '(' + @columnListNaked + ')' + SPACE(1) + CHAR(10)
                                + 'SELECT' + SPACE(1) + @columnListNaked + SPACE(1) + 'FROM [dbo].' + @tempTableName + SPACE(1) + CHAR(10)
                                + 'WITH (NOWAIT)' + SPACE(1)
                                + 'OPTION' + SPACE(1) + '(MAXDOP' + SPACE(1) + CAST(@maxDOP AS nvarchar(1)) + ');' + SPACE(1) + CHAR(10)
                                + 'SET IDENTITY_INSERT [dbo].' + @tableName + SPACE(1) + 'OFF;' + SPACE(1) + CHAR(10)
                                + 'SET STATISTICS TIME OFF;';
                            END
                            ELSE
                                SET @sql = 'SET STATISTICS TIME ON;' + SPACE(1) + CHAR(10)
                                + 'INSERT INTO [dbo].' + @tableName + SPACE(1) + @parallelInsert + '(' + @columnListNaked + ')' + SPACE(1) + CHAR(10)
                                + 'SELECT' + SPACE(1) + @columnListNaked + SPACE(1) + 'FROM [dbo].' + @tempTableName + SPACE(1) + CHAR(10)
                                + 'WITH (NOWAIT)' + SPACE(1)
                                + 'OPTION' + SPACE(1) + '(MAXDOP' + SPACE(1) + CAST(@maxDOP AS nvarchar(1)) + ');' + SPACE(1) + CHAR(10)
                                + 'SET STATISTICS TIME OFF;';

                            SET @PrintMsg = @sql;
                            RAISERROR (N'%s', 0, 1, @PrintMsg) WITH NOWAIT;
                            --WAITFOR DELAY '00:00:01'
                            IF (@verbose = 1)
                            BEGIN
                                SET @stepNo = @stepNo + 1;
                                SELECT
                                    @isLastStep = COUNT(1)
                                FROM [dbo].ConversionLog
                                WHERE [dbo].ConversionLog.[TableName] = @tableName
                                AND [dbo].ConversionLog.[StepNo] = @stepNo;

                                IF (@isLastStep = 0)
                                BEGIN
                                    EXECUTE [dbo].[CommandExecute] N'',
                                                                   @sql,
                                                                   1,@Result OUTPUT;
									SET @sql = CONCAT('INSERT INTO [dbo].ConversionLog(TableName, StepNo, ExecuteCmd, ProcessedTime, IsLastStep)',SPACE(1),
													  'VALUES',SPACE(1),'(','''',@tableName,'''',',',@stepNo,',','''',REPLACE(@sql,'''',''''''),'''',',','''',GETDATE(),'''',',',0,');')
                                    IF (@Result <> 0)
                                        EXECUTE [dbo].[CommandExecute] N'',
                                                                       @sql,
                                                                       1,@Result OUTPUT;
                                    --IF (@Result <> 0)
                                    --	INSERT [dbo].ConversionLog (TableName, StepNo, ExecuteCmd, ProcessedTime, IsLastStep)
                                    --		VALUES (@tableName, @stepNo, @sql, GETDATE(), 0);
                                    RAISERROR ('', 0, 1) WITH NOWAIT;
                                END
                                ELSE
                                    RAISERROR (@MsgPrevString, 0, 1, @stepNo) WITH NOWAIT
                            END

                            IF (@disableNonClustered = 1)
                                IF (LEN(@indexName) <> 0)
                                BEGIN
                                    --- Enable back Cluster and NonCluster Index
                                    SET @sql = 'SET STATISTICS TIME ON;' + SPACE(1)
                                    + 'ALTER INDEX ALL ON' + SPACE(1) + @tableName + SPACE(1) + 'REBUILD;' + SPACE(1)
                                    + 'SET STATISTICS TIME OFF;';
                                    SET @PrintMsg = @sql;
                                    RAISERROR (N'%s', 0, 1, @PrintMsg) WITH NOWAIT;
                                    IF (@verbose = 1)
                                    BEGIN
                                        SET @stepNo = @stepNo + 1;
                                        SELECT
                                            @isLastStep = COUNT(1)
                                        FROM [dbo].ConversionLog
                                        WHERE [dbo].ConversionLog.[TableName] = @tableName
                                        AND [dbo].ConversionLog.[StepNo] = @stepNo;

                                        IF (@isLastStep = 0)
                                        BEGIN
                                            EXECUTE [dbo].[CommandExecute] N'',
                                                                           @sql,
                                                                           1,@Result OUTPUT;
											SET @sql = CONCAT('INSERT INTO [dbo].ConversionLog(TableName, StepNo, ExecuteCmd, ProcessedTime, IsLastStep)',SPACE(1),
															  'VALUES',SPACE(1),'(','''',@tableName,'''',',',@stepNo,',','''',REPLACE(@sql,'''',''''''),'''',',','''',GETDATE(),'''',',',0,');')
                                            IF (@Result <> 0)
                                                EXECUTE [dbo].[CommandExecute] N'',
                                                                               @sql,
                                                                               1,@Result OUTPUT;
                                            --IF (@Result <> 0)
                                            --	INSERT [dbo].ConversionLog (TableName, StepNo, ExecuteCmd, ProcessedTime, IsLastStep)
                                            --		VALUES (@tableName, @stepNo, @sql, GETDATE(), 0);
                                            RAISERROR ('', 0, 1) WITH NOWAIT;
                                        END
                                        ELSE
                                            RAISERROR (@MsgPrevString, 0, 1, @stepNo) WITH NOWAIT
                                    END
                                END
                            -- Enable back constraint after transfer the data
                            SET @sql = 'ALTER TABLE' + SPACE(1) + @tableName + SPACE(1) + 'CHECK CONSTRAINT ALL;'
                            SET @PrintMsg = @sql;
                            RAISERROR (N'%s', 0, 1, @PrintMsg) WITH NOWAIT;
                            IF (@verbose = 1)
                            BEGIN
                                SET @stepNo = @stepNo + 1;
                                SELECT
                                    @isLastStep = COUNT(1)
                                FROM [dbo].ConversionLog
                                WHERE [dbo].ConversionLog.[TableName] = @tableName
                                AND [dbo].ConversionLog.[StepNo] = @stepNo;

                                IF (@isLastStep = 0)
                                BEGIN
                                    EXECUTE [dbo].[CommandExecute] N'',
                                                                   @sql,
                                                                   1,@Result OUTPUT;
									SET @sql = CONCAT('INSERT INTO [dbo].ConversionLog(TableName, StepNo, ExecuteCmd, ProcessedTime, IsLastStep)',SPACE(1),
													  'VALUES',SPACE(1),'(','''',@tableName,'''',',',@stepNo,',','''',REPLACE(@sql,'''',''''''),'''',',','''',GETDATE(),'''',',',0,');')
                                    IF (@Result <> 0)
                                        EXECUTE [dbo].[CommandExecute] N'',
                                                                       @sql,
                                                                       1,@Result OUTPUT;
                                --IF (@Result <> 0)
                                --	INSERT [dbo].ConversionLog (TableName, StepNo, ExecuteCmd, ProcessedTime, IsLastStep)
                                --		VALUES (@tableName, @stepNo, @sql, GETDATE(), 0);
                                END
                                ELSE
                                    RAISERROR (@MsgPrevString, 0, 1, @stepNo) WITH NOWAIT
                            END
                            -- Drop the table after transfer the data

                            SET @sql = 'DROP TABLE [dbo].' + @tempTableName + ';';
                            SET @PrintMsg = @sql;
                            RAISERROR (N'%s', 0, 1, @PrintMsg) WITH NOWAIT;
                            IF (@verbose = 1)
                            BEGIN
                                SET @stepNo = @stepNo + 1;
                                SELECT
                                    @isLastStep = COUNT(1)
                                FROM [dbo].ConversionLog
                                WHERE [dbo].ConversionLog.[TableName] = @tableName
                                AND [dbo].ConversionLog.[StepNo] = @stepNo;

                                IF (@isLastStep = 0)
                                BEGIN
                                    EXECUTE [dbo].[CommandExecute] N'',
                                                                   @sql,
                                                                   1,@Result OUTPUT;
									SET @sql = CONCAT('INSERT INTO [dbo].ConversionLog(TableName, StepNo, ExecuteCmd, ProcessedTime, IsLastStep)',SPACE(1),
													  'VALUES',SPACE(1),'(','''',@tableName,'''',',',@stepNo,',','''',REPLACE(@sql,'''',''''''),'''',',','''',GETDATE(),'''',',',1,');')
                                    IF (@Result <> 0)
                                        EXECUTE [dbo].[CommandExecute] N'',
                                                                       @sql,
                                                                       1,@Result OUTPUT;
                                --IF (@Result <> 0)
                                --	INSERT [dbo].ConversionLog (TableName, StepNo, ExecuteCmd, ProcessedTime, IsLastStep)
                                --		VALUES (@tableName, @stepNo, @sql, GETDATE(), 1);
                                END
                                ELSE
                                    RAISERROR (@MsgPrevString, 0, 1, @stepNo) WITH NOWAIT
                            END
                        END
                    END
                END
                FETCH NEXT FROM tbl_cursor INTO @tableNo, @table, @rowCount
            END
            CLOSE tbl_cursor
            DEALLOCATE tbl_cursor

            RAISERROR ('', 0, 1) WITH NOWAIT;
            SET @PrintMsg = '-- Changing CodePage is done for' + SPACE(1) + @company + '.'
            RAISERROR (N'%s', 0, 1, @PrintMsg) WITH NOWAIT;
            SET @EndDate = GETDATE();
            SELECT
                @Duration = CONVERT(varchar(5), DATEDIFF(s, @StartDatePerComp, @EndDate) / 3600) + ':'
                + CONVERT(varchar(5), DATEDIFF(s, @StartDatePerComp, @EndDate) % 3600 / 60) + ':'
                + CONVERT(varchar(5), (DATEDIFF(s, @StartDatePerComp, @EndDate) % 60))

            SET @PrintMsg = '-- Start Time :' + CONVERT(varchar(20), @StartDatePerComp, 120);
            RAISERROR (N'%s', 0, 1, @PrintMsg) WITH NOWAIT;
            SET @PrintMsg = '-- End Time :' + CONVERT(varchar(20), @EndDate, 120);
            RAISERROR (N'%s', 0, 1, @PrintMsg) WITH NOWAIT;
            SET @PrintMsg = '-- Total Duration :' + SPACE(1) + @Duration
            RAISERROR (N'%s', 0, 1, @PrintMsg) WITH NOWAIT;

            RAISERROR ('------------------------------------------------------------------', 0, 1) WITH NOWAIT;

            FETCH NEXT FROM cmp_cursor INTO @company
        END
        CLOSE cmp_cursor
        DEALLOCATE cmp_cursor

        SET @EndDate = GETDATE();
        SELECT
            @Duration = CONVERT(varchar(5), DATEDIFF(s, @StartDate, @EndDate) / 3600) + ':'
            + CONVERT(varchar(5), DATEDIFF(s, @StartDate, @EndDate) % 3600 / 60) + ':'
            + CONVERT(varchar(5), (DATEDIFF(s, @StartDate, @EndDate) % 60))

        RAISERROR ('', 0, 1) WITH NOWAIT;
        SET @PrintMsg = '-- Start Time :' + CONVERT(varchar(20), @StartDate, 120);
        RAISERROR (@PrintMsg, 0, 1) WITH NOWAIT;
        SET @PrintMsg = '-- End Time :' + CONVERT(varchar(20), @EndDate, 120);
        RAISERROR (@PrintMsg, 0, 1) WITH NOWAIT;
        SET @PrintMsg = '-- Overall Duration :' + SPACE(1) + @Duration
        RAISERROR (@PrintMsg, 0, 1) WITH NOWAIT;

        IF (@verbose = 1)
        BEGIN
            SELECT
                *
            FROM [dbo].ConversionLog;
            IF OBJECT_ID('[dbo].ConversionLog') IS NOT NULL
                DROP TABLE [dbo].ConversionLog;
        END
        --Enable the Auto Update Statistics
        IF (@verbose = 1)
		    EXECUTE [dbo].[CommandExecute] N'',N'ALTER DATABASE CURRENT SET AUTO_UPDATE_STATISTICS ON;',0,@Result OUTPUT;
    END TRY
    BEGIN CATCH
		--Enable the Auto Update Statistics
        IF (@verbose = 1)
            EXECUTE [dbo].[CommandExecute] N'',N'ALTER DATABASE CURRENT SET AUTO_UPDATE_STATISTICS ON;',0,@Result OUTPUT;
		THROW;
    END CATCH
    END
END
GO