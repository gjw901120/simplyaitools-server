package com.simply.ai.server.manager.manager.impl;

import com.simply.ai.server.manager.manager.OrderRefundManager;
import com.simply.ai.server.manager.mapper.OrderRefundMapper;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;

@Component
public class OrderRefundManagerImpl implements OrderRefundManager {

    @Resource
    private OrderRefundMapper orderRefundMapper;

}