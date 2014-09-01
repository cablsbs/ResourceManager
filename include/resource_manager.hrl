-ifndef(RESOURCE_MANAGER_HRL_).
-define(RESOURCE_MANAGER_HRL_, 1).

-define(ADD, 1).    % 新增
-define(DEL, 2).    % 删除
-define(MOD, 3).    % 修改

-record(diff_info, {file_name, diff_type}).

-endif.
