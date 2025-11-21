package com.simply.ai.server.manager.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.simply.ai.server.manager.entity.Order;
import org.apache.ibatis.annotations.Mapper;

@Mapper
public interface OrderMapper extends BaseMapper<Order> {
}