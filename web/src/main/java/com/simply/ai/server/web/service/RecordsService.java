package com.simply.ai.server.web.service;

import com.simply.ai.server.manager.entity.UserModelTask;

import java.util.List;
import java.util.Map;

public interface RecordsService {

    String create(String model, UserModelTask userModelTask);

    Boolean completed(String taskId, List<String> outputUrl, Object outputCallbackDetails);

    Boolean failed(String taskId, Object outputCallbackDetails);
}
