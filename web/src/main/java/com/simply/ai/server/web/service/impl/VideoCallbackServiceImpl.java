package com.simply.ai.server.web.service.impl;

import com.simply.ai.server.web.model.dto.request.callback.video.*;
import com.simply.ai.server.web.service.VideoCallbackService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

/**
 * 视频回调服务实现
 */
@Service
@Slf4j
public class VideoCallbackServiceImpl implements VideoCallbackService {


    @Override
    public void VeoCallback(VeoCallbackRequest request) {
        VeoCallbackData data = request.getData();

        log.info("Veo回调处理完成, taskId: {}", data.getTaskId());

    }

    @Override
    public void RunwayCallback(RunwayCallbackRequest request) {
        RunwayCallbackData data = request.getData();

        log.info("Runway回调处理完成, taskId: {}", data.getTaskId());

    }

    @Override
    public void RunwayAlephCallback(RunwayAlephCallbackRequest request) {
        RunwayAlephCallbackData data = request.getData();

        log.info("RunwayAleph回调处理完成, taskId: {}", request.getTaskId());

    }

    @Override
    public void LumaCallback(LumaCallbackRequest request) {
        LumaCallbackData data = request.getData();

        log.info("Luma回调处理完成, taskId: {}", data.getTaskId());

    }

    @Override
    public void SoraCallback(SoraCallbackRequest request) {
        SoraCallbackData data = request.getData();

        log.info("Sora回调处理完成, taskId: {}", data.getTaskId());

    }

}