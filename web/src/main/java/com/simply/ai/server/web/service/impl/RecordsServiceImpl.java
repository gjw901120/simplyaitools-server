package com.simply.ai.server.web.service.impl;

import com.simply.ai.server.manager.entity.UserModelRecords;
import com.simply.ai.server.manager.entity.UserModelTask;
import com.simply.ai.server.manager.manager.ModelsManager;
import com.simply.ai.server.manager.manager.UserModelRecordsManager;
import com.simply.ai.server.manager.manager.UserModelTaskManager;
import com.simply.ai.server.web.model.dto.response.BaseResponse;
import com.simply.ai.server.web.service.RecordsService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.HashMap;

@Service
public class RecordsServiceImpl implements RecordsService {

    @Autowired
    private UserModelRecordsManager userModelRecordsManager;

    @Autowired
    private UserModelTaskManager userModelTaskManager;

    @Autowired
    private ModelsManager modelsManager;

    @Override
    @Transactional
    public String create(String model, UserModelTask userModelTask) {
        //根据模型名称获取id
        Integer modelId = modelsManager.getModelIdByName(model);

        UserModelRecords userModelRecords = UserModelRecords.create(0, modelId);

        //写入记录
        userModelRecordsManager.insert(userModelRecords);

        userModelTask.setRecordId(userModelRecords.getUuid());
        userModelTask.setModelId(modelId);

        //写入任务
        userModelTaskManager.insert(userModelTask);

        return userModelRecords.getUuid();
    }
}
