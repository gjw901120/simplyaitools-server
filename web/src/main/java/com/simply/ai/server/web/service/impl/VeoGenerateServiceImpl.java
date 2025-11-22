package com.simply.ai.server.web.service.impl;

import com.simply.ai.server.manager.entity.UserModelRecords;
import com.simply.ai.server.manager.entity.UserModelTask;
import com.simply.ai.server.manager.enums.ResponseCodeEnum;
import com.simply.ai.server.manager.manager.VideoManager;
import com.simply.ai.server.manager.model.response.VideoGenerateResponse;
import com.simply.ai.server.manager.model.request.VeoGenerateRequest;
import com.simply.ai.server.manager.model.request.VeoExtendRequest;
import com.simply.ai.server.web.model.dto.request.VeoExtendDTO;
import com.simply.ai.server.web.model.dto.request.VeoGenerateDTO;
import com.simply.ai.server.web.model.dto.response.BaseResponse;
import com.simply.ai.server.web.service.RecordsService;
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
    private RecordsService recordsService;

    @Override
    public BaseResponse generateVideo(VeoGenerateDTO veoGenerateDTO) {
        // 实现视频生成逻辑
        VeoGenerateRequest request = new VeoGenerateRequest();

        BeanUtils.copyProperties(veoGenerateDTO, request);

        List<String> inputUrls = new ArrayList<>();

        request.setImageUrls(inputUrls);

        VideoGenerateResponse response = videoManager.veoGenerate(request);

        if(!ResponseCodeEnum.SUCCESS.equals(response.getCode())) {
            throw new BaseException(ThirdpartyErrorType.THIRDPARTY_SERVER_ERROR, response.getMsg());
        }

        //写入任务
        UserModelTask userModelTask = UserModelTask.create(
                0,
                "",
                0,
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

        return new BaseResponse(recordsService.create(veoGenerateDTO.getModel(), userModelTask));

    }

    @Override
    public BaseResponse extendVideo(VeoExtendDTO veoExtendDTO) {

        VeoExtendRequest request =  new VeoExtendRequest();

        BeanUtils.copyProperties(veoExtendDTO, request);

        VideoGenerateResponse response = videoManager.veoExtend(request);

        if(!ResponseCodeEnum.SUCCESS.equals(response.getCode())) {
            throw new BaseException(ThirdpartyErrorType.THIRDPARTY_SERVER_ERROR, response.getMsg());
        }

        //写入任务
        UserModelTask userModelTask = UserModelTask.create(
                0,
                "",
                0,
                0,
                1,
                "",
                response.getData().getTaskId(),
                new ArrayList<>(),
                new ArrayList<>(),
                request,
                new HashMap<>(),
                new HashMap<>()
        );

        return new BaseResponse(recordsService.create("veo3_extend", userModelTask));

    }

}