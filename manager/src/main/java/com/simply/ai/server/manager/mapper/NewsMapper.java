package com.simply.ai.server.manager.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.simply.ai.server.manager.entity.News;
import org.apache.ibatis.annotations.Mapper;

@Mapper
public interface NewsMapper extends BaseMapper<News> {
}