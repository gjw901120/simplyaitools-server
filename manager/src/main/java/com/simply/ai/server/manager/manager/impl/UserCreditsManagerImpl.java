package com.simply.ai.server.manager.manager.impl;

import com.simply.ai.server.manager.entity.UserCredits;
import com.simply.ai.server.manager.manager.UserCreditsManager;
import com.simply.ai.server.manager.mapper.UserCreditsMapper;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;

@Component
public class UserCreditsManagerImpl implements UserCreditsManager {

    @Resource
    private UserCreditsMapper userCreditsMapper;

    @Override
    public Integer insert(UserCredits userCredits) {
        userCreditsMapper.insert(userCredits);
        return userCredits.getId();
    }

}