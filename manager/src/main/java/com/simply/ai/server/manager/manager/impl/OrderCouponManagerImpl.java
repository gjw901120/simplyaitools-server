package com.simply.ai.server.manager.manager.impl;

import com.simply.ai.server.manager.manager.OrderCouponManager;
import com.simply.ai.server.manager.mapper.OrderCouponMapper;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;

@Component
public class OrderCouponManagerImpl implements OrderCouponManager {

    @Resource
    private OrderCouponMapper orderCouponMapper;

}
