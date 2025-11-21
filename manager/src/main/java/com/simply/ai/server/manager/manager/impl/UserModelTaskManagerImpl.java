package com.simply.ai.server.manager.manager.impl;

import com.simply.ai.server.manager.entity.UserModelTask;
import com.simply.ai.server.manager.manager.UserModelTaskManager;
import com.simply.ai.server.manager.mapper.UserModelTaskMapper;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;

@Component
public class UserModelTaskManagerImpl implements UserModelTaskManager {

    @Resource
    private UserModelTaskMapper userModelTaskMapper;

    @Override
    public Integer insert(UserModelTask userModelTask) {
        userModelTaskMapper.insert(userModelTask);
        return userModelTask.getId();
    }


}