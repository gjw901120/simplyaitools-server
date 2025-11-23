package com.simply.ai.server.manager.manager;

import com.simply.ai.server.manager.entity.UserModelRecords;

public interface UserModelRecordsManager {

    String insert(UserModelRecords userModelRecords);

    UserModelRecords getDetailIdByUuId(String Uuid);

    Integer updateById(UserModelRecords userModelRecords);

}
