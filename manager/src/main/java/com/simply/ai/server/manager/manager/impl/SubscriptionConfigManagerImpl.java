package com.simply.ai.server.manager.manager.impl;

import com.simply.ai.server.manager.manager.SubscriptionConfigManager;
import com.simply.ai.server.manager.mapper.SubscriptionConfigMapper;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;

@Component
public class SubscriptionConfigManagerImpl implements SubscriptionConfigManager {

    @Resource
    private SubscriptionConfigMapper subscriptionConfigMapper;

}