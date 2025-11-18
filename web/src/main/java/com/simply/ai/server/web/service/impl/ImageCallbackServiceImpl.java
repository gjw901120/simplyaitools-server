package com.simply.ai.server.web.service.impl;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.simply.ai.server.web.model.dto.request.callback.image.*;
import com.simply.ai.server.web.service.ImageCallbackService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

/**
 * 图像回调服务实现
 */
@Slf4j
@Service
@RequiredArgsConstructor
public class ImageCallbackServiceImpl implements ImageCallbackService {

    private final ObjectMapper objectMapper;

    @Override
    public void processGpt4oCallback(ImageGpt4oCallbackRequest request) {

        String taskId = request.getData().getTaskId();
        Integer code = request.getCode();

        log.info("Processing GPT-4O image callback: taskId={}, code={}", taskId, code);

        if (code == 200) {
            // 成功处理
            String[] resultUrls = request.getData().getInfo().getResultUrls();
            log.info("GPT-4O image generation completed: taskId={}, resultUrls={}", taskId, resultUrls);

            updateTaskStatus(taskId, "SUCCESS", resultUrls, null);

        } else {
            log.info("Failed to process GPT-4O callback: taskId={}, info={}", request.getData().getTaskId(), request);
        }

    }

    @Override
    public void processFluxKontextCallback(ImageFluxKontextCallbackRequest request) {

        String taskId = request.getData().getTaskId();
        Integer code = request.getCode();

        log.info("Processing Flux Kontext image callback: taskId={}, code={}", taskId, code);

        if (code == 200) {
            // 成功处理
            String originImageUrl = request.getData().getInfo().getOriginImageUrl();
            String resultImageUrl = request.getData().getInfo().getResultImageUrl();

            log.info("Flux Kontext image generation completed: taskId={}, originUrl={}, resultUrl={}", taskId, originImageUrl, resultImageUrl);

            updateTaskStatus(taskId, "SUCCESS", new String[]{resultImageUrl}, null);

        } else {
            log.info("Failed to process Flux Kontext callback: taskId={}, info={}", request.getData().getTaskId(), request);
        }

    }

    @Override
    public void processNanoBananaCallback(ImageNanoBananaCallbackRequest request) {
        try {
            String taskId = request.getData().getTaskId();
            String state = request.getData().getState();
            String resultJson = request.getData().getResultJson();

            log.info("Processing Nano Banana image callback: taskId={}, state={}", taskId, state);

            if ("success".equals(state)) {
                // 解析结果
                ImageNanoBananaCallbackRequest.NanoBananaResult result =
                        objectMapper.readValue(resultJson, ImageNanoBananaCallbackRequest.NanoBananaResult.class);

                log.info("Nano Banana image generation completed: taskId={}, resultUrls={}", taskId, result.getResultUrls());

                updateTaskStatus(taskId, "SUCCESS", result.getResultUrls(), null);

                // 记录积分消耗
                recordCreditConsumption(taskId, request.getData().getConsumeCredits());

            } else if ("fail".equals(state)) {
                log.info("Nano Banana image generation failed: taskId={}, info={}", taskId, request);

                updateTaskStatus(taskId, "FAILED", null, "Image generation failed");
            }

        } catch (Exception e) {
            log.error("Failed to process Nano Banana callback: taskId={}, error={}", request.getData().getTaskId(), e);
        }
    }

    /**
     * 更新任务状态
     */
    private void updateTaskStatus(String taskId, String status, Object result, String errorMessage) {
        // TODO: 实现更新任务状态的逻辑
        log.info("Updating image task status: taskId={}, status={}, result={}, error={}",
                taskId, status, result, errorMessage);
    }

    /**
     * 记录积分消耗
     */
    private void recordCreditConsumption(String taskId, Integer credits) {
        // TODO: 实现记录积分消耗的逻辑
        log.info("Recording image task credit consumption: taskId={}, credits={}", taskId, credits);
    }
}