package com.simply.ai.server.manager.manager.impl;

import com.simply.ai.server.manager.entity.SubscriptionPlan;
import com.simply.ai.server.manager.manager.SubscriptionPlanManager;
import com.simply.ai.server.manager.mapper.SubscriptionPlanMapper;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;

@Component
public class SubscriptionPlanManagerImpl implements SubscriptionPlanManager {

    @Resource
    private SubscriptionPlanMapper subscriptionPlanMapper;

    @Override
    public Integer insert(SubscriptionPlan subscriptionPlan) {
        subscriptionPlanMapper.insert(subscriptionPlan);
        return subscriptionPlan.getId();
    }

}