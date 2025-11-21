package com.simply.ai.server.manager.manager.impl;

import com.simply.ai.server.manager.entity.Bill;
import com.simply.ai.server.manager.manager.BillManager;
import com.simply.ai.server.manager.mapper.BillMapper;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;

@Component
public class BillManagerImpl implements BillManager {

    @Resource
    private BillMapper billMapper;

    @Override
    public Integer insert(Bill bill) {
        billMapper.insert(bill);
        return bill.getId();
    }

}
