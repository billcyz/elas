-define(SERVER_URL, "/elas/").
-define(DEFAULT_HTTP_PORT, 8080).
-define(PROJECT_ETS, [project, project_url, url_content, url_action]).

%% http://<address>/example
%% http://<address>/example/sample_01
%% http://<address>/example/sample_02
%% http://<address>/example/sample_03
%% http://<address>/example/sample_04
-define(SAMPLE_SERVICE, example).
-define(SAMPLE_URL_01, "/sample_01").
-define(SAMPLE_URL_02, "/sample_02").
-define(SAMPLE_URL_03, "/sample_03").
-define(SAMPLE_URL_04, "/sample_04").

-define(SAMPLE_01_ACTION, "GET").
-define(SAMPLE_02_ACTION, "GET").
-define(SAMPLE_03_ACTION, "POST").
-define(SAMPLE_04_ACTION, "POST").
