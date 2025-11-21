package com.simply.ai.server.manager.manager.impl;

import com.simply.ai.server.manager.entity.UserModelConversion;
import com.simply.ai.server.manager.manager.UserModelConversionManager;
import com.simply.ai.server.manager.mapper.UserModelConversionMapper;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;

@Component
public class UserModelConversionManagerImpl implements UserModelConversionManager {

    @Resource
    private UserModelConversionMapper userModelConversionMapper;

    @Override
    public Integer insert(UserModelConversion userModelConversion) {
        userModelConversionMapper.insert(userModelConversion);
        return userModelConversion.getId();
    }

}