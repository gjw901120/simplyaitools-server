package com.simply.ai.server.manager.manager.impl;

import com.simply.ai.server.manager.entity.UserModelRecords;
import com.simply.ai.server.manager.manager.UserModelRecordsManager;
import com.simply.ai.server.manager.mapper.UserModelRecordsMapper;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;

@Component
public class UserModelRecordsManagerImpl implements UserModelRecordsManager {

    @Resource
    private UserModelRecordsMapper userModelRecordsMapper;

    @Override
    public String insert(UserModelRecords userModelTask) {
        userModelRecordsMapper.insert(userModelTask);
        return userModelTask.getUuid();
    }

}