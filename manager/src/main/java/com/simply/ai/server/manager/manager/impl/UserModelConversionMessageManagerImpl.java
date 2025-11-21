package com.simply.ai.server.manager.manager.impl;

import com.simply.ai.server.manager.entity.UserModelConversionMessage;
import com.simply.ai.server.manager.manager.UserModelConversionMessageManager;
import com.simply.ai.server.manager.mapper.UserModelConversionMessageMapper;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;

@Component
public class UserModelConversionMessageManagerImpl implements UserModelConversionMessageManager {

    @Resource
    private UserModelConversionMessageMapper userModelConversionMessageMapper;

    @Override
    public Integer insert(UserModelConversionMessage userModelConversionMessage) {
        userModelConversionMessageMapper.insert(userModelConversionMessage);
        return userModelConversionMessage.getId();
    }

}