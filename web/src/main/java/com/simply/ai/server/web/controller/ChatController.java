package com.simply.ai.server.web.controller;

import com.simply.ai.server.web.model.dto.request.chat.ChatgptConversionDTO;
import com.simply.ai.server.web.model.dto.request.chat.ClaudeConversionDTO;
import com.simply.ai.server.web.model.dto.request.chat.DeepseekConversionDTO;
import com.simply.ai.server.web.model.dto.request.chat.GeminiConversionDTO;
import com.simply.ai.server.web.service.ChatConversionService;
import com.simply.common.core.entity.vo.ResponseResult;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.http.codec.ServerSentEvent;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import reactor.core.publisher.Flux;

import javax.validation.Valid;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

@Slf4j
@RestController
@RequestMapping("/api/chat")
@RequiredArgsConstructor
public class ChatController {

    @Autowired
    private ChatConversionService  chatConversionService;

    @PostMapping(value ="/chatgpt", produces = MediaType.TEXT_EVENT_STREAM_VALUE)
    public Flux<ServerSentEvent<Object>> gptConversion(@RequestBody @Valid ChatgptConversionDTO request) {

        log.info("ChatGPT流式请求，模型: {}, 问题: {}", request.getModel(), request.getPrompt());

        return chatConversionService.processChatgptStream(request)
                .map(response -> {
                    Map<String, Object> eventData = new HashMap<>();
                    // 根据响应类型处理
                    String eventType = response.getType();
                    eventData.put("type", eventType);
                    // 如果是内容增量事件
                    if ("response.output_text.delta".equals(eventType)) {
                        eventData.put("delta", response.getDelta());
                    } else if ("response.completed".equals(eventType)) {
                        // 完成事件，包含使用情况
                        eventData.put("is_final", true);
                        if (response.getResponse() != null && response.getResponse().getUsage() != null) {
                            eventData.put("usage", response.getResponse().getUsage());
                        }
                    } else if ("response.content_part.done".equals(eventType)) {
                        // 内容部分完成，可能包含完整文本
                        if (response.getPart() != null) {
                            eventData.put("text", response.getPart().getText());
                        }
                    }
                    // 可以添加更多事件类型的处理

                    return ServerSentEvent.<Object>builder()
                            .data(eventData)
                            .event(eventType)
                            .id(response.getSequenceNumber() != null ? response.getSequenceNumber().toString() : null)
                            .build();
                })
                .onErrorResume(e -> {
                    log.error("ChatGPT流式处理失败", e);
                    return Flux.just(ServerSentEvent.<Object>builder()
                            .data(Map.of("error", e.getMessage()))
                            .event("error")
                            .build());
                });
    }

    @PostMapping(value ="/claude", produces = MediaType.TEXT_EVENT_STREAM_VALUE)
    public Flux<ServerSentEvent<Object>> claudeConversion(@RequestBody @Valid ClaudeConversionDTO request) {

        log.info("Claude流式请求，模型: {}, 问题: {}", request.getModel(), request.getPrompt());

        return chatConversionService.processClaudeStream(request)
                .map(aggregatedEvent -> {
                    Map<String, Object> eventData = new HashMap<>();

                    // 始终包含累积内容
                    eventData.put("content", aggregatedEvent.getContent());

                    // 如果是增量内容，包含delta
                    if (aggregatedEvent.getDelta() != null) {
                        eventData.put("delta", aggregatedEvent.getDelta());
                    }

                    // 是否最终块
                    eventData.put("is_final", aggregatedEvent.getIsFinal());

                    // 如果是最终块，包含使用情况和停止原因
                    if (Boolean.TRUE.equals(aggregatedEvent.getIsFinal())) {
                        if (aggregatedEvent.getUsage() != null) {
                            eventData.put("usage", aggregatedEvent.getUsage());
                        }
                        if (aggregatedEvent.getStopReason() != null) {
                            eventData.put("stop_reason", aggregatedEvent.getStopReason());
                        }
                    }

                    return ServerSentEvent.<Object>builder()
                            .data(eventData)
                            .event("message")
                            .id(UUID.randomUUID().toString())
                            .build();
                })
                .onErrorResume(e -> {
                    log.error("Claude流式处理失败", e);
                    return Flux.just(ServerSentEvent.<Object>builder()
                            .data(Map.of("error", e.getMessage()))
                            .event("error")
                            .build());
                });
    }

    @PostMapping("/gemini")
    public ResponseResult<?> geminiConversion(@RequestBody @Valid GeminiConversionDTO request) {

        return ResponseResult.success();
    }

    @PostMapping(value ="/deepseek", produces = MediaType.TEXT_EVENT_STREAM_VALUE)
    public Flux<ServerSentEvent<Object>> deepseekConversion(@RequestBody @Valid DeepseekConversionDTO request) {
        log.info("DeepSeek流式请求，模型: {}, 问题: {}", request.getModel(), request.getPrompt());

        return chatConversionService.processDeepseekStream(request)
                .map(response -> {
                    Map<String, Object> eventData = new HashMap<>();

                    // 添加增量内容
                    if (response.getChoices() != null && !response.getChoices().isEmpty() &&
                            response.getChoices().get(0).getDelta() != null) {
                        String content = response.getChoices().get(0).getDelta().getContent();
                        eventData.put("content", content);
                    }

                    // 如果是最终块，添加统计信息
                    if (Boolean.TRUE.equals(response.getIsFinal())) {
                        eventData.put("is_final", true);
                        eventData.put("finish_reason", response.getFinishReason());

                        if (response.getUsage() != null) {
                            eventData.put("usage", response.getUsage());
                        }

                        if (response.getSearchResults() != null) {
                            eventData.put("search_results", response.getSearchResults());
                        }
                    } else {
                        eventData.put("is_final", false);
                    }

                    return ServerSentEvent.<Object>builder()
                            .data(eventData)
                            .event("message")
                            .id(response.getId())
                            .build();
                })
                .onErrorResume(e -> {
                    log.error("流式处理失败", e);
                    return Flux.just(ServerSentEvent.<Object>builder()
                            .data(Map.of("error", e.getMessage()))
                            .event("error")
                            .build());
                });
    }

}
