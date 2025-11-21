package com.simply.ai.server.manager.manager.impl;

import com.simply.ai.server.manager.entity.User;
import com.simply.ai.server.manager.manager.UserManager;
import com.simply.ai.server.manager.mapper.UserMapper;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;

@Component
public class UserManagerImpl implements UserManager {

    @Resource
    private UserMapper userMapper;

    @Override
    public Integer insert(User user) {
        userMapper.insert(user);
        return user.getId();
    }

}