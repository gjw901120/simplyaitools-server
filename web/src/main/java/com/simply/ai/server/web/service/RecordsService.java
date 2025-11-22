package com.simply.ai.server.web.service;

import com.simply.ai.server.manager.entity.UserModelRecords;
import com.simply.ai.server.manager.entity.UserModelTask;

public interface RecordsService {

    String create(String model, UserModelTask userModelTask);
}
