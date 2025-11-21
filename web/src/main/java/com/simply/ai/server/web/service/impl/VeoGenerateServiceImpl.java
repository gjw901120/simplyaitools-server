package com.simply.ai.server.web.service.impl;

import com.simply.ai.server.manager.entity.UserModelRecords;
import com.simply.ai.server.manager.entity.UserModelTask;
import com.simply.ai.server.manager.enums.ResponseCodeEnum;
import com.simply.ai.server.manager.manager.ModelsManager;
import com.simply.ai.server.manager.manager.UserModelRecordsManager;
import com.simply.ai.server.manager.manager.UserModelTaskManager;
import com.simply.ai.server.manager.manager.VideoManager;
import com.simply.ai.server.manager.model.response.VideoGenerateResponse;
import com.simply.ai.server.manager.model.request.VeoGenerateRequest;
import com.simply.ai.server.manager.model.request.VeoExtendRequest;
import com.simply.ai.server.web.model.dto.request.VeoExtendDTO;
import com.simply.ai.server.web.model.dto.request.VeoGenerateDTO;
import com.simply.ai.server.web.model.dto.response.BaseResponse;
import com.simply.ai.server.web.service.VeoGenerateService;
import com.simply.common.core.exception.BaseException;
import com.simply.common.core.exception.error.ThirdpartyErrorType;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.*;


/**
 * Veo视频生成服务实现
 */
@Service
public class VeoGenerateServiceImpl implements VeoGenerateService {

    @Autowired
    private VideoManager videoManager;

    @Autowired
    private UserModelRecordsManager userModelRecordsManager;

    @Autowired
    private UserModelTaskManager userModelTaskManager;

    @Autowired
    private ModelsManager modelsManager;

    @Override
    public BaseResponse generateVideo(VeoGenerateDTO veoGenerateDTO) {
        // 实现视频生成逻辑
        VeoGenerateRequest request = new VeoGenerateRequest();

        BeanUtils.copyProperties(veoGenerateDTO, request);

        VideoGenerateResponse response = videoManager.veoGenerate(request);

        if(!ResponseCodeEnum.SUCCESS.equals(response.getCode())) {
            throw new BaseException(ThirdpartyErrorType.THIRDPARTY_SERVER_ERROR, response.getMsg());
        }

        //根据模型名称获取id
        Integer modelId = modelsManager.getModelIdByName(veoGenerateDTO.getModel());

        //写入记录
        UserModelRecords userModelRecords = UserModelRecords.create(0, modelId);
        userModelRecordsManager.insert(userModelRecords);

        List<String> inputUrls = new ArrayList<>();
        //写入任务
        UserModelTask userModelTask = UserModelTask.create(
                0,
                userModelRecords.getUuid(),
                modelId,
                0,
                1,
                "",
                response.getData().getTaskId(),
                inputUrls,
                new ArrayList<>(),
                request,
                new HashMap<>(),
                new HashMap<>()
        );
        userModelTaskManager.insert(userModelTask);

        return new BaseResponse(userModelRecords.getUuid());

    }

    @Override
    public void extendVideo(VeoExtendDTO veoExtendDTO) {

        VeoExtendRequest request =  new VeoExtendRequest();

        BeanUtils.copyProperties(veoExtendDTO, request);

        VideoGenerateResponse response = videoManager.veoExtend(request);

        if(!ResponseCodeEnum.SUCCESS.equals(response.getCode())) {
            throw new BaseException(ThirdpartyErrorType.THIRDPARTY_SERVER_ERROR, response.getMsg());
        }

    }

}