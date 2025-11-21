package com.simply.ai.server.manager.manager.impl;

import com.simply.ai.server.manager.entity.Order;
import com.simply.ai.server.manager.manager.OrderManager;
import com.simply.ai.server.manager.mapper.OrderMapper;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;

@Component
public class OrderManagerImpl implements OrderManager {

    @Resource
    private OrderMapper orderMapper;

    @Override
    public Integer insert(Order order) {
        orderMapper.insert(order);
        return order.getId();
    }

}